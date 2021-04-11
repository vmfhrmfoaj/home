;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package cider
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'cider nil t))

  (defvar cider--select-jack-in-history nil)

  (defun cider-custom-jack-in ()
    (interactive)
    (let* ((type (completing-read "Select CIDER jack-in type: "
                                  '(Clojure Both ClojureScript)
                                  nil nil nil 'cider--select-jack-in-history
                                  (car cider--select-jack-in-history)))
           (jack-in-fn (cond
                        ((string= "Clojure" type)
                         #'cider-jack-in-clj)
                        ((string= "Both" type)
                         #'cider-jack-in-clj&cljs)
                        ((string= "ClojureScript" type)
                         #'cider-jack-in-cljs)))
           (cur-ns (clojure-find-ns))
           (cider-connected-hook-orig (and (boundp 'cider-connected-hook)
                                           cider-connected-hook)))
      (when (eq jack-in-fn 'cider-jack-in-clj)
        (add-hook 'cider-connected-hook
                  (lambda ()
                    (cider-repl-set-ns cur-ns)
                    (with-eval-after-load "cider-eval"
                      (run-at-time 3 nil ; FIXME
                                   (lambda (buf)
                                     (with-current-buffer buf
                                       (cider-interactive-eval ":update-cider-repl-ns-cache")))
                                   (current-buffer)))
                    (setq cider-connected-hook cider-connected-hook-orig))))
      (call-interactively jack-in-fn)))

  (defalias 'cider-switch-to-releated-repl-buffer 'cider-custom-jack-in)

  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point
                          font-lock-fontify-region-function #'font-lock-default-fontify-region)))

  :config
  (defun cider--repl-buf (&optional ignore-repl-type)
    (when (cider-connected-p)
      (let* ((repl-type (cider-connection-type-for-buffer))
             (root (clojure-project-root-path))
             (repl-buf (->> (cider-repls (unless ignore-repl-type repl-type))
                            ;; NOTE
                            ;;  `cider-jack-in-clj&cljs' create two buffer and one of them will be changed CLJS REPL.
                            ;;  but until browser connecte to REPL, it is CLJ REPL.  so, I recognize CLJS REPL heuristically,
                            ;;  a length of a buffer name of CLJS REPL is always longer than a buffer name of CLJ REPL.
                            (--sort (string-lessp (buffer-name it) (buffer-name other)))
                            (--first
                             (with-current-buffer it
                               (string-equal root (clojure-project-root-path)))))))
        repl-buf)))

  (defun cider--set-repl-ns-to-current-ns (&optional repl)
    (when-let ((repl (or repl (cider--repl-buf))))
      (let ((cur-ns (clojure-find-ns))
            (repl-ns (buffer-local-value 'cider-buffer-ns repl)))
        (when (and (stringp cur-ns) (stringp repl-ns)
                   (not (string= cur-ns (buffer-local-value 'cider-buffer-ns repl))))
          (cider-repl-set-ns cur-ns)))))

  (defun cider-switch-to-releated-repl-buffer ()
    (interactive)
    (if-let ((repl (cider--repl-buf)))
        (progn
          (cider--set-repl-ns-to-current-ns repl)
          (cider--switch-to-repl-buffer repl nil))
      (let ((bufs (-some->> (projectile-project-buffers)
                            (--filter (let ((buf-name (buffer-name it)))
                                        (or (s-starts-with? "*cider-repl" buf-name)
                                            (s-starts-with? "*nrepl-server" buf-name)))))))
        (cond
         ((null bufs)
          (cider-custom-jack-in))
         (t (message
             (concat "It seems that a REPL is in initialization phase, "
                     "please check \"*nrepl-server ...*\" buffer.")))))))

  (defun cider-hard-restart (&optional repl)
    (interactive)
    (let* ((repl (or repl
                     (sesman-browser-get 'object)
                     (cider-current-repl nil 'ensure))))
      (cider-quit repl)
      (cider-jack-in nil)))

  (defun cider-find-var-at-point ()
    (interactive)
    (when (cider-connected-p)
      (cider-find-var nil (cider-symbol-at-point))))

  (setq cider-mode-line-show-connection nil
        cider-mode-line '(:eval (unless (ignore-errors (cider-current-repl))
                                  (propertize " CIDER[not connected]" 'face 'error)))
        cider-use-fringe-indicators nil)

  (cider-register-cljs-repl-type
   'figwheel-custom
   (concat "(do "
           "" "(while (:auto-refresh-lock (ns-interns 'clojure.core))"
           ""   "(Thread/sleep 500)) "
           "" "(if (find-var 'clojure.core/auto-refresh-lock)"
           "" "  (locking clojure.core/auto-refresh-lock "
           ""     "(require 'dev.repl) "
           ""     "(dev.repl/start))"
           "" "  (do"
           ""     "(require 'dev.repl) "
           ""     "(dev.repl/start))))"))

  (advice-add #'cider-restart :around
              (lambda (fn &rest args)
                "Wrap `cider-restart' to keep current namespace of REPL."
                (let ((cur-ns (cider-current-ns)))
                  (apply fn args)
                  (cider-repl-set-ns cur-ns)))))

(use-package cider-client
  :defer t
  :init
  (eval-when-compile (require 'cider-client nil t))

  :config
  (advice-add #'cider-fallback-eval:classpath :override
              (lambda ()
                "fix a bug that occur when `*print-length*' is not `nil'."
                (let ((classpath (thread-first
                                     (concat
                                      "(binding [*print-level* nil, *print-length* nil]"
                                      "  (print (seq (.split (System/getProperty \"java.class.path\") \":\"))))")
                                   (cider-sync-tooling-eval)
                                   (nrepl-dict-get "value")
                                   (read)))
                      (project (clojure-project-dir)))
                  (mapcar (lambda (path) (cider--get-abs-path path project)) classpath)))))

(use-package cider-eval
  :defer t
  :init
  (eval-when-compile (require 'cider-eval nil t))

  :config
  (defconst cider-clojure-1.10.1-raw-error
    `(sequence
      "CompilerException Syntax error "
      (minimal-match (zero-or-more anything))
      (any "compiling" "macroexpanding" ) space
      (minimal-match (zero-or-more anything))
      "at ("
      (group-n 2 (minimal-match (zero-or-more anything)))
      ":"
      (group-n 3 (one-or-more digit))
      (optional ":" (group-n 4 (one-or-more digit)))
      ")."))

  (defconst cider-clojure-compilation-regexp
    (eval
     `(rx bol (or ,cider-clojure-1.10.1-raw-error
                  ,cider-clojure-1.10-error
                  ,cider-clojure-1.9-error
                  ,cider-clojure-warning))))

  (defun cider-handle-compilation-errors-for-flycheck (msg eval-buf)
    (when (string-match-p "^:reloading (" msg)
      (setq clojure--compilation-errors nil))
    (when-let ((info (cider-extract-error-info cider-compilation-regexp msg)))
      (add-to-list 'clojure--compilation-errors (cons eval-buf info))
      (with-current-buffer eval-buf
        (when (bound-and-true-p flycheck-mode)
          (flycheck-buffer)))))

  (setq cider-compilation-regexp
        (list cider-clojure-compilation-regexp
              2 3 4 '(1)))

  (with-eval-after-load "flycheck"
    (advice-add #'cider-handle-compilation-errors :override #'cider-handle-compilation-errors-for-flycheck)))

(use-package cider-mode
  :defer t
  :init
  (eval-when-compile (require 'cider-mode nil t))

  :config
  (defun cider-doc-at-point ()
    (interactive)
    (cider-doc-lookup (cider-symbol-at-point)))

  (setq cider-dynamic-indentation t
        cider-font-lock-dynamically '(deprecated)))

(use-package cider-repl
  :defer t
  :init
  (eval-when-compile (require 'cider-repl nil t))

  :config
  (defvar clojure--compilation-error-ns nil)

  (defun cider-repl-catch-compilation-error-ns (_buf msg)
    (when (string-match "^:error-while-loading \\([-_.0-9A-Za-z]+\\)" msg)
      (setq clojure--compilation-error-ns (match-string-no-properties 1 msg))))

  (defun cider-repl-catch-compilation-error (_buf msg)
    (when (string-match-p "^:reloading (" msg)
      (setq clojure--compilation-errors nil))
    (when-let ((info (cider-extract-error-info cider-compilation-regexp msg)))
      (let* ((file (if (null clojure--compilation-error-ns)
                       (nth 0 info)
                     (concat (->> clojure--compilation-error-ns
                                  (string-replace "-" "_")
                                  (string-replace "." "/"))
                             "." (file-name-extension (nth 0 info)))))
             (root (->> (or nrepl-project-dir
                            (clojure-project-root-path))
                        (file-truename)
                        (s-chop-suffix "/")))
             (buf (or (get-file-buffer (concat root "/src/"  file))
                      (get-file-buffer (concat root "/test/" file))
                      (-some->> (cider-sync-request:classpath)
                                (--filter (s-starts-with? (file-truename nrepl-project-dir) it))
                                (--map (get-file-buffer (concat it "/" file)))
                                (-non-nil)
                                (-first-item)))))
        (when buf
          (add-to-list 'clojure--compilation-errors (cons buf info))
          (with-current-buffer buf
            (when (bound-and-true-p flycheck-mode)
              (flycheck-buffer))))))
    (setq clojure--compilation-error-ns nil))

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (eldoc-mode 1)
              (company-mode 1)
              (let ((f (lambda ()
                         (when eldoc-mode
                           (run-at-time 0.01 nil #'eldoc-refresh)))))
                (add-hook 'evil-insert-state-entry-hook f nil t)
                (add-hook 'evil-insert-state-exit-hook  f nil t))))

  (advice-add #'cider-repl-emit-stdout :after #'cider-repl-catch-compilation-error-ns)
  (advice-add #'cider-repl-emit-stderr :after #'cider-repl-catch-compilation-error)
  (advice-add #'cider-repl-return :after
              (lambda (&optional _end-of-input)
                "Change evil state to normal."
                (evil-normal-state))))

(use-package clojure-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'clojure-mode nil t))

  :config
  (defvar clojure-font-lock-extend-regex
    (concat "(" (regexp-opt '("def" "extend" "proxy" "reify"))))

  (defun clojure-font-lock-kw-at-point (point)
    (goto-char point)
    (ignore-errors
      (beginning-of-defun))
    (let ((beg-kw (point)))
      (when (and (not (= point beg-kw))
                 (looking-at clojure-font-lock-extend-regex))
        (ignore-errors
          ;; move forward as much as possible until failure (or success)
          (forward-char)
          (dotimes (_ 4)
            (forward-sexp)))
        (cons beg-kw (point)))))

  (defvar clojure--compilation-errors nil)

  (defun clojure--flycheck-start (checker callback)
    (->> clojure--compilation-errors
         (--map (let* ((buf  (car it))
                       (info (cdr it))
                       (file (nth 0 info))
                       (line (nth 1 info))
                       (col  (nth 2 info))
                       (face (nth 3 info))
                       (note (nth 4 info))
                       (msg (when (string-match "{.*}" note)
                              (match-string 0 note)))
                       (lv (cond
                            ((eq face 'cider-warning-highlight-face) 'warning)
                            ((eq face 'cider-error-highlight-face) 'error)
                            (t 'info)))
                       (end-col (if (null buf)
                                    (1+ col)
                                  (with-current-buffer buf
                                    (save-excursion
                                      (goto-line line)
                                      (move-to-column col)
                                      (forward-symbol 1)
                                      (point))))))
                  (flycheck-error-new
                   :buffer (or buf (current-buffer))
                   :checker checker
                   :filename (file-name-nondirectory file)
                   :message (or msg note)
                   :level lv
                   :id "Syntax error"
                   :line line
                   :column col
                   :end-column end-col)))
         (funcall callback 'finished)))

  (defun clojure-skip (&rest items)
    "TODO"
    (let* ((l items))
      (while l
        (skip-chars-forward " \r\t\n")
        (cond
         ((eq (car l) :comment)
          (if (not (looking-at-p ";"))
              (setq l (cdr l))
            (setq l (-remove-item :comment items))
            (comment-forward (point-max))))
         ((eq (car l) :metadata)
          (if (not (looking-at-p "#?\\^"))
              (setq l (cdr l))
            (setq l (-remove-item :metadata items))
            (forward-sexp 1)))
         ((eq (car l) :ignored-form)
          (if (not (looking-at-p "#_\\|,"))
              (setq l (cdr l))
            (setq l (-remove-item :ignored-form items))
            (or (/= 0 (skip-chars-forward ","))
                (forward-sexp (if (looking-at-p "#_\\s(") 2 1)))
            (clojure-skip :ignored-form)))
         ((eq (car l) :tagged-literal)
          (if (not (looking-at-p "#[0-9A-Za-z]"))
              (setq l (cdr l))
            (setq l (-remove-item :tagged-literal items))
            (forward-sexp 1)
            (clojure-skip :tagged-literal)))
         ((eq (car l) :destructuring-bind)
          (if (not (looking-at-p "{\\|\\["))
              (setq l (cdr l))
            (setq l (-remove-item :destructuring-bind items))
            (forward-sexp 1)
            (when (car l)
              (apply #'clojure-skip l))
            (forward-sexp 1)
            (clojure-skip :destructuring-bind)))
         ((eq (car l) :string)
          (if (not (looking-at-p "\""))
              (setq l (cdr l))
            (setq l (-remove-item :string items))
            (forward-sexp 1)
            (clojure-skip :string)))
         ((eq (car l) :map)
          (if (not (looking-at-p "{"))
              (setq l (cdr l))
            (setq l (-remove-item :map items))
            (forward-sexp 1)
            (clojure-skip :map)))
         ((eq (car l) :vector)
          (if (not (looking-at-p "\\["))
              (setq l (cdr l))
            (setq l (-remove-item :vector items))
            (forward-sexp 1)
            (clojure-skip :vector)))
         (t (setq l (cdr l)))))))

  (defun clojure-forward-sexp (&optional n)
    (or n (setq n 1))
    (while (< 0 n)
      (clojure-skip :comment :ignored-form :tagged-literal)
      (forward-sexp 1)
      (setq n (1- n))))

  (defun clojure--custom-not-function-form-p ()
    "Customize `clojure--not-function-form-p' function for return type-hint."
    (or (member (char-after) '(?\[ ?\{))
        (save-excursion ;; Catch #?@ (:cljs ...)
          (skip-chars-backward "\r\n[:blank:]")
          (when (eq (char-before) ?@)
            (forward-char -1))
          (and (eq (char-before) ?\?)
               (eq (char-before (1- (point))) ?\#)))
        ;; Car of form is not a symbol.
        (not (looking-at ".\\(?:\\^[0-9A-Za-z]+\\s-+\\)?\\(?:\\sw\\|\\s_\\)"))))

  (defcustom clojure-clj-spec-reqs
    '([clojure.spec.alpha :as spec])
    "TODO")

  (defcustom clojure-cljs-spec-reqs
    '([cljs.spec.alpha :as spec])
    "TODO")

  (defcustom clojure-clj-test-reqs
    '([clojure.test :refer :all])
    "TODO")

  (defcustom clojure-cljs-test-reqs
    '([cljs.test :refer-macros [async deftest is testing]])
    "TODO")

  (defcustom clojure-cljs-test-ns-meta
    '(^:figwheel-load)
    "TODO")

  (defun clojure-insert-namespace ()
    (interactive)
    (let* ((ns (if buffer-file-name
                   (if (fboundp 'cider-expected-ns)
                       (cider-expected-ns)
                     (clojure-expected-ns))
                 "user.ns"))
           (buf-file-name (or buffer-file-name ""))
           (test-file? (string-match-p "_test\\.clj[cs]?$" buf-file-name))
           (spec-file? (string-match-p "_spec\\.clj[cs]?$" buf-file-name))
           (cljc-file? (string-match-p "\\.cljc$" buf-file-name))
           (cljs-file? (string-match-p "\\.cljs$" buf-file-name))
           (req-vecs (lambda (reqs)
                       (-interpose '(edn-raw . "\n")
                                   (if test-file?
                                       (cons (vector (make-symbol (s-replace-regexp "-test$" "" ns)) :as 'target) reqs)
                                     reqs))))
           (clj-spec-reqs  (funcall req-vecs clojure-clj-spec-reqs))
           (clj-test-reqs  (funcall req-vecs clojure-clj-test-reqs))
           (cljs-spec-reqs (funcall req-vecs clojure-cljs-spec-reqs))
           (cljs-test-reqs (funcall req-vecs clojure-cljs-test-reqs)))
      (goto-char (point-min))
      (insert
       (edn-print-string
        `(ns ,@(when (and test-file? cljs-file?)
                 clojure-cljs-test-ns-meta)
             ,(make-symbol ns)
             ,@(when (or test-file?
                         spec-file?)
                 `((edn-raw . "\n")
                   (:require
                    ,@(cond
                       (test-file?
                        (cond
                         (cljc-file? `((edn-reader-macro
                                        "#?@" (:clj (edn-raw . "") [,@clj-test-reqs]
                                                    (edn-raw . "\n") :cljs [,@cljs-test-reqs]))))
                         (cljs-file? cljs-test-reqs)
                         (t clj-test-reqs)))
                       (spec-file?
                        (cond
                         (cljc-file? `((edn-reader-macro
                                        "#?@" (:clj (edn-raw . "") [,@clj-spec-reqs]
                                                    (edn-raw . "\n") :cljs [,@cljs-spec-reqs]))))
                         (cljs-file? cljs-spec-reqs)
                         (t clj-spec-reqs)))))))))
       "\n")
      (delete-trailing-whitespace (point-min) (point))
      (when buffer-file-name
        (save-buffer))))

  (with-eval-after-load "flycheck"
    (unless (flycheck-valid-checker-p 'clj-cider-repl)
      (flycheck-define-generic-checker
          'clj-cider-repl
        "A syntax checker using the Cider REPL provided."
        :start #'clojure--flycheck-start
        :modes '(clojure-mode clojurescript-mode clojurec-mode)
        :predicate (lambda ()
                     (when (fboundp #'cider-connected-p)
                       (cider-connected-p)))))
    (add-to-list 'flycheck-checkers 'clj-cider-repl))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (prettify-symbols-mode 1)
              (when (and buffer-file-name (= (point-min) (point-max)))
                (clojure-insert-namespace))
              (let ((f (lambda ()
                         (when eldoc-mode
                           (run-at-time 0.01 nil #'eldoc-refresh)))))
                (add-hook 'evil-insert-state-entry-hook f nil t)
                (add-hook 'evil-insert-state-exit-hook  f nil t))
              (when (require 'flycheck nil t)
                (add-hook 'after-save-hook
                          (lambda ()
                            (setq clojure--compilation-errors nil))
                          -100 t)
                (flycheck-mode)
                (if (featurep 'flycheck-clj-kondo)
                    (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
                      (flycheck-add-next-checker checker 'clj-cider-repl))
                  (flycheck-select-checker 'clj-cider-repl))))
            :append)

  (advice-add #'clojure-font-lock-def-at-point :override #'clojure-font-lock-kw-at-point)
  (advice-add #'clojure--not-function-form-p :override #'clojure--custom-not-function-form-p))

(use-package edn
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'edn nil t))

  :config
  (defun edn-raw-p (raw)
    (and (consp raw)
         (eq 'edn-raw (car raw))))

  (defun edn-raw (raw)
    (format "%s" (cdr raw)))

  (defun edn-reader-macro-p (reader-macro)
    (and (listp reader-macro)
         (eq 'edn-reader-macro (car reader-macro))))

  (defun edn-reader-macro (reader-macro)
    (let ((reader-macro (cadr reader-macro))
          (form (caddr reader-macro)))
      (concat reader-macro (edn-print-string form))))

  (edn-add-writer #'edn-raw-p #'edn-raw)
  (edn-add-writer #'edn-reader-macro-p #'edn-reader-macro))

(use-package flycheck-clj-kondo
  :ensure t
  :after (clojure-mode flycheck)
  :init
  (eval-when-compile (require 'flycheck-clj-kondo nil t))

  :config
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc))
    (setf (flycheck-checker-get checker 'error-filter)
          (lambda (errs)
            ;; for `Meander'
            (--remove (string-match-p "Unresolved symbol: \\?" (flycheck-error-message it))
                      errs)))))

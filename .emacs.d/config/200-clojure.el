;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package cider
  :ensure t
  :defer t
  :config
  (defun cider-get-repl-buf ()
    (when (cider-connected-p)
      (let* ((repl-type (cider-connection-type-for-buffer))
             (root (clojure-project-root-path))
             (all-repl-bufs (cider-connections))
             (prj-repl-bufs (--filter (with-current-buffer it
                                        (string-equal root (clojure-project-root-path)))
                                      all-repl-bufs))
             (repl-buf (or (--first (with-current-buffer it
                                      (string-equal repl-type cider-repl-type))
                                    prj-repl-bufs)
                           (-first-item prj-repl-bufs))))
        repl-buf)))

  (defun cider-set-repl-ns-to-current-ns (&optional repl)
    (when-let ((repl (or repl (cider-get-repl-buf))))
      (let ((cur-ns (clojure-find-ns))
            (repl-ns (buffer-local-value 'cider-buffer-ns repl)))
        (when (and (stringp cur-ns) (stringp repl-ns)
                   (not (string= cur-ns (buffer-local-value 'cider-buffer-ns repl))))
          (cider-repl-set-ns cur-ns)))))

  (defun cider-switch-to-releated-repl-buffer ()
    (interactive)
    (if-let ((repl (cider-get-repl-buf)))
        (progn
          (cider-set-repl-ns-to-current-ns repl)
          (cider--switch-to-repl-buffer repl nil))
      (message "Cider didn't start")))

  (defun cider-hard-restart (&optional repl)
    (interactive)
    (let* ((repl (or repl
                     (sesman-browser-get 'object)
                     (cider-current-repl nil 'ensure))))
      (cider-quit repl)
      (cider-jack-in nil)))

  (defun cider-find-var-at-point ()
    (interactive)
    (if (cider-connected-p)
        (cider-find-var nil (cider-symbol-at-point))
      (dumb-jump-go)))

  (defun cider-switch-to-last-clj-buf ()
    (interactive)
    (let ((display-fn (if cider-repl-display-in-current-window
                          #'pop-to-buffer-same-window
                        #'pop-to-buffer))
          (mode-vals (if (string= "clj" cider-repl-type)
                         '((clojure-mode . 2) (clojurescript-mode . 1))
                       '((clojurescript-mode . 2) (clojure-mode . 1)))))
      (-some->> (or (projectile-project-buffers) (buffer-list))
        (sort-buffer-by-visit-time)
        (--sort (let ((itv    (with-current-buffer it    (alist-get major-mode mode-vals 0)))
                      (otherv (with-current-buffer other (alist-get major-mode mode-vals 0))))
                  (> itv otherv)))
        (-first-item)
        (funcall display-fn))))

  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (setq-local font-lock-fontify-region-function #'font-lock-default-fontify-region)
              (eldoc-mode 1)))

  (defvar cider-buffer-list-update-timer nil)

  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (and cider-mode
                         (or (eq major-mode 'clojure-mode)
                             (eq major-mode 'clojurescript-mode)
                             (eq major-mode 'clojurec-mode)))
                (let ((buf (current-buffer)))
                  (when (timerp cider-buffer-list-update-timer)
                    (cancel-timer cider-buffer-list-update-timer))
                  ;; NOTE
                  ;;  after executing `projectile-find-file', i don't know why `buffer-list-update-hook' was called multiple times in short period of a time.
                  ;;  below timer is workaround to avoid to call `cider-set-repl-ns-to-current-ns' multiple times.
                  (setq cider-buffer-list-update-timer
                        (run-with-timer 0.1 nil
                                        (lambda ()
                                          (with-current-buffer buf
                                            (cider-set-repl-ns-to-current-ns)))))))))

  (advice-add #'cider--close-connection :before
              (lambda (repl &rest _)
                "I use my own Clojure REPL middleware, it create a temporary file.
`cider--close-connection' send SIGKILL signal, JVM can't handle this signal.
So, the middleware can't remove this file. I use this workaround until fixing the problem."
                (let ((params (cider--gather-connect-params nil repl)))
                  (when-let ((proj-dir (plist-get params :project-dir)))
                    (delete-file (concat proj-dir "/.auto-refresh"))))))

  (advice-add #'cider-restart :around
              (lambda (fn &rest args)
                "Wrap `cider-restart' to keep current namespace of REPL."
                (let ((cur-ns (cider-current-ns)))
                  (apply fn args)
                  (cider-repl-set-ns cur-ns)))))

(use-package cider-eval
  :defer t
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
  :config
  (defun cider-doc-at-point ()
    (interactive)
    (cider-doc-lookup (cider-symbol-at-point)))

  (setq cider-dynamic-indentation nil
        cider-font-lock-dynamically '(deprecated))

  (add-hook 'cider-connected-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point))))

(use-package cider-repl
  :defer t
  :config
  (defun cider-repl-catch-compilation-error (_buf msg)
    (when-let ((info (cider-extract-error-info cider-compilation-regexp msg)))
      (let* ((file (nth 0 info))
             (root (->> (or nrepl-project-dir
                            (clojure-project-root-path))
                        (file-truename)
                        (s-chop-suffix "/")))
             (buf (or (get-file-buffer (concat root "/src/"  file))
                      (get-file-buffer (concat root "/test/" file))
                      (-some->> (cider-sync-request:classpath)
                        (--filter (s-starts-with? nrepl-project-dir it))
                        (--map (get-file-buffer (concat it "/" file)))
                        (-non-nil)
                        (-first-item)))))
        (add-to-list 'clojure--compilation-errors (cons buf info))
        (when buf
          (with-current-buffer buf
            (when (bound-and-true-p flycheck-mode)
              (flycheck-buffer)))))))

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (eldoc-mode 1)
              (company-mode 1)))

  (advice-add #'cider-repl-emit-stderr :after #'cider-repl-catch-compilation-error))

(use-package clojure-mode
  :ensure t
  :defer t
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

  (defvar clojure--compilation-errors '())

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

  (advice-add #'clojure-font-lock-def-at-point :override #'clojure-font-lock-kw-at-point))

(use-package edn
  :ensure t
  :defer t
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
  :after (clojure-mode flycheck))

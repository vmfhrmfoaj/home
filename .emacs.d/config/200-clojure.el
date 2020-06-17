(use-package cider
  :disabled t
  :ensure t
  :defer t
  :config
  (defn cider-cljs-root-dirs ()
    "TODO"
    (let (repl reconn? cur-ns)
      (dolist (buf (cider-connections) repl)
        (with-current-buffer buf
          (when (string= "clj" cider-repl-type)
            (setq repl buf))))
      (unless repl
        (setq repl (car (cider-connections))
              reconn? t)
        (when repl
          ;; How to get a current namespace in RPEL
          ;; (setq cur-ns (cider-current-ns))
          (nrepl-sync-request:eval ":cljs/quit" repl)))
      (prog1
          (when repl
            (ignore-errors
              (-some-> (concat "(let [b (some-> \"project.clj\""
                               "                (slurp)"
                               "                (clojure.edn/read-string)"
                               "                (some->> (drop 3) (apply hash-map))"
                               "                (get-in [:cljsbuild :builds]))"
                               "      b (cond-> b (map? b) (vals))]"
                               "  (some->> b"
                               "           (map :source-paths)"
                               "           (flatten)"
                               "           (apply hash-set)"
                               "           (map clojure.java.io/as-file)"
                               "           (map (memfn getAbsolutePath))))")
                       (nrepl-sync-request:eval repl)
                       (nrepl-dict-get "value")
                       (edn-read))))
        (when (and repl reconn?)
          (nrepl-sync-request:eval cider-cljs-lein-repl repl)
          (and cur-ns (cider-repl-set-ns cur-ns))))))

  (defn cider-conn-type-for-cljc-buffer (&optional buf)
    "TODO"
    (with-current-buffer (or buf (current-buffer))
      (when (eq 'clojurec-mode major-mode)
        (let ((cur-pos (point)))
          (save-excursion
            (when (re-search-backward "#\\?@?(" nil t)
              (let ((beg-pos (prog1 (point) (forward-sexp)))
                    (end-pos (point)))
                (when (< beg-pos cur-pos end-pos)
                  (goto-char cur-pos)
                  (when (re-search-backward "\\(?:^\\s-*\\|(\\):\\(cljs?\\)[ \r\t\n]+"  beg-pos t)
                    (match-string-no-properties 1))))))))))

  (defn cider-switch-to-releated-repl-buffer (&optional set-namespace)
    "TODO"
    (interactive "P")
    (let* ((repl-type (cider-connection-type-for-buffer))
           (root (clojure-project-root-path))
           (all-repl-bufs (cider-connections))
           (prj-repl-bufs (--filter (with-current-buffer it
                                      (string-equal root (clojure-project-root-path)))
                                    all-repl-bufs))
           (buffer (--first (with-current-buffer it
                              (string-equal repl-type cider-repl-type))
                            (or prj-repl-bufs all-repl-bufs))))
      (cider--switch-to-repl-buffer (or buffer
                                        (-first-item prj-repl-bufs)
                                        (-first-item all-repl-bufs))
                                    set-namespace)))

  (defn cider-switch-to-last-clj-buf ()
    "TODO"
    (interactive)
    (let ((display-fn (if cider-repl-display-in-current-window
                          #'pop-to-buffer-same-window
                        #'pop-to-buffer))
          (target-modes (if (string= "clj" cider-repl-type)
                            '(clojure-mode clojurescript-mode)
                          '(clojurescript-mode clojure-mode))))
      (while target-modes
        (let ((target-mode (car target-modes)))
          (setq target-modes (cdr target-modes))
          (when (-some->> (buffer-list)
                          (--filter (with-current-buffer it
                                      (or (eq target-mode    major-mode)
                                          (eq 'clojurec-mode major-mode))))
                          (--sort (let ((it    (or (buf-visit-time it    'all) 0))
                                        (other (or (buf-visit-time other 'all) 0)))
                                    (time-less-p other it)))
                          (-first-item)
                          (funcall display-fn))
            (setq target-modes nil))))))

  (defn cider-expected-ns-for-cljs (path)
    "TODO"
    (-when-let (root-dirs (and (cider-connected-p)
                               (string= "cljs" (cider-connection-type-for-buffer))
                               (cider-cljs-root-dirs)))
      (let* ((path (or path (file-truename (buffer-file-name))))
             (relpath (->> root-dirs
                           (--filter (string-prefix-p it path))
                           (--sort (< (length it) (length other)))
                           (-first-item))))
        (when relpath
          (->> path
               (string-remove-prefix (concat relpath "/"))
               (file-name-sans-extension)
               (replace-regexp-in-string "/" ".")
               (replace-regexp-in-string "_" "-"))))))

  (defn cider-find-var-at-point ()
    "TODO"
    (interactive)
    (if (cider-connected-p)
        (cider-find-var nil (cider-symbol-at-point))
      (dumb-jump-go)))

  (defn cider-doc-at-point ()
    "TODO"
    (interactive)
    (cider-doc-lookup (cider-symbol-at-point)))

  (setq cider-repl-display-in-current-window t
        cider-repl-use-pretty-printing t
        cider-cljs-lein-repl (concat "(do"
                                     "  (require 'figwheel-sidecar.repl-api)"
                                     "  (figwheel-sidecar.repl-api/start-figwheel!)"
                                     "  (figwheel-sidecar.repl-api/cljs-repl))"))
  (advice-add #'cider-connection-type-for-buffer :before-until #'cider-conn-type-for-cljc-buffer)
  (advice-add #'cider-switch-to-repl-buffer :override #'cider-switch-to-releated-repl-buffer)
  (advice-add #'cider-switch-to-last-clojure-buffer :override #'cider-switch-to-last-clj-buf)
  (advice-add #'cider-expected-ns :before-until #'cider-expected-ns-for-cljs)
  (add-hook 'cider-connected-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point))))

(use-package cider-mode
  :disabled t
  :defer t
  :config
  (setq cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil
        cider-mode-line '(:eval (when (cider-connected-p)
                                  (format " cider(%s)" (cider--modeline-info)))))
  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (setq-local font-lock-fontify-region-function
                          #'font-lock-default-fontify-region)
              (eldoc-mode 1))))

(use-package cider-repl
  :disabled t
  :defer t
  :config
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (eldoc-mode 1))))

(use-package clojure-mode
  :disabled t
  :ensure t
  :defer t
  :config
  (defn clojure-skip (direction-or-item &rest items)
    "TODO"
    (let* ((direction (if (and (numberp direction-or-item) (> 0 direction-or-item)) (- 1) (+ 1)))
           (items     (if (numberp direction-or-item) items (cons direction-or-item items)))
           (l items))
      (while l
        (skip-chars-forward " \r\t\n")
        (cond
         ((eq (car l) :comment)
          (if (not (looking-at-p ";"))
              (setq l (cdr l))
            (setq l (-remove-item :comment items))
            (comment-forward (point-max))))
         ((eq (car l) :type-hint)
          (if (not (looking-at-p "\\^"))
              (setq l (cdr l))
            (setq l (-remove-item :type-hint items))
            (forward-sexp direction)))
         ((eq (car l) :ignored-form)
          (if (not (looking-at-p "#_\\|,"))
              (setq l (cdr l))
            (setq l (-remove-item :ignored-form items))
            (or (/= 0 (skip-chars-forward ","))
                (forward-sexp (if (looking-at-p "#_\\s(") (* direction 2) (* direction 1))))
            (clojure-skip :ignored-form)))
         ((eq (car l) :tagged-literal)
          (if (not (looking-at-p "#[0-9A-Za-z]"))
              (setq l (cdr l))
            (setq l (-remove-item :tagged-literal items))
            (forward-sexp direction)
            (clojure-skip :tagged-literal)))
         ((eq (car l) :destructuring-bind)
          (if (not (looking-at-p "{\\|\\["))
              (setq l (cdr l))
            (setq l (-remove-item :destructuring-bind items))
            (forward-sexp direction)
            (when (car l)
              (apply #'clojure-skip l))
            (forward-sexp direction)
            (clojure-skip :destructuring-bind)))
         ((eq (car l) :string)
          (if (not (looking-at-p "\""))
              (setq l (cdr l))
            (setq l (-remove-item :string items))
            (forward-sexp direction)
            (clojure-skip :string)))
         ((eq (car l) :map)
          (if (not (looking-at-p "{"))
              (setq l (cdr l))
            (setq l (-remove-item :map items))
            (forward-sexp direction)
            (clojure-skip :map)))
         ((eq (car l) :vector)
          (if (not (looking-at-p "\\["))
              (setq l (cdr l))
            (setq l (-remove-item :vector items))
            (forward-sexp direction)
            (clojure-skip :vector)))
         (t (setq l (cdr l)))))))

  (defn clojure-correct-font-lock-syntatic-face (res)
    "(def _ \"abc\")<~ It should be the string."
    (if (and (eq res font-lock-doc-face)
             (save-excursion
               (goto-char (nth 1 state))
               (not (looking-at-p "(defprotocol\\>")))
             (let ((list-end (save-excursion
                               (goto-char (nth 1 state))
                               (forward-sexp)
                               (1- (point))))
                   (str-end  (save-excursion
                               (goto-char (nth 8 state))
                               (forward-sexp)
                               (clojure-skip :comment :ignored-form)
                               (point))))
               (= list-end str-end)))
        font-lock-string-face
      res))

  (defvar clojure-context-fn-prefix-regex (regexp-opt '("eval" "run" "exe" "execute"))
    "TODO")

  (defvar clojure-context-fn-regex (regexp-opt '("in" "on" "with"))
    "TODO")

  (defn clojure--get-indentation (sym)
    "TODO"
    (when (or (let ((case-fold-search nil))
                (string-match-p "^[-_A-Z]+\\>" sym))
              (string-match-p (concat "^\\("
                                      clojure-context-fn-prefix-regex "-\\)?"
                                      clojure-context-fn-regex
                                      "\\>")
                              sym))
      :defn))

  (defconst clojure-namespace-name-regex-1
    (rx line-start "("
        (zero-or-one (zero-or-one "clojure.") "core/")
        (zero-or-one "in-") "ns" (zero-or-one "+")
        (one-or-more (any whitespace "\n")))
    "TODO")

  (defn clojure-find-ns-custom ()
    "Sometimes `clojure-find-ns' is slow due to repeatedly call `up-list' to find the top level.
This is customized version of `clojure-find-ns' to improve some performance."
    (save-excursion
      (save-restriction
        (widen)
        (let ((limit (line-end-position))
              matched?)
          (goto-char (point-min))
          (while (re-search-forward clojure-namespace-name-regex-1 limit t)
            (setq matched? t))
          (when matched?
            (goto-char (match-beginning 0))
            (when (re-search-forward clojure-namespace-name-regex nil t)
              (match-string-no-properties 4)))))))

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

  (defn clojure-insert-namespace ()
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

  (defn clojure-setup ()
    "TODO"
    (setq-local font-lock-extend-region-functions
                (remove 'clojure-font-lock-extend-region-def
                        font-lock-extend-region-functions))
    (when (and buffer-file-name (= (point-min) (point-max)))
      (clojure-insert-namespace)))

  (setq clojure-get-indent-function #'clojure--get-indentation)
  (put-clojure-indent 'lazy-seq 0)
  (add-hook 'clojure-mode-hook #'clojure-setup :append)
  (advice-add #'clojure-find-ns :override #'clojure-find-ns-custom)
  (advice-add #'clojure-font-lock-syntactic-face-function :filter-return
              #'clojure-correct-font-lock-syntatic-face))

(use-package edn
  :disabled t
  :ensure t
  :defer t
  :config
  (defn edn-raw-p (raw)
    (and (consp raw)
         (eq 'edn-raw (car raw))))

  (defn edn-raw (raw)
    (format "%s" (cdr raw)))

  (defn edn-reader-macro-p (reader-macro)
    (and (listp reader-macro)
         (eq 'edn-reader-macro (car reader-macro))))

  (defn edn-reader-macro (reader-macro)
    (let ((reader-macro (cadr reader-macro))
          (form (caddr reader-macro)))
      (concat reader-macro (edn-print-string form))))

  (edn-add-writer #'edn-raw-p #'edn-raw)
  (edn-add-writer #'edn-reader-macro-p #'edn-reader-macro))

(use-package helm-cider-cheatsheet
  :disabled t
  :ensure helm-cider
  :defer t)

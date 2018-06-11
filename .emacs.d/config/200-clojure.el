(use-package cider
  :ensure t
  :defer t
  :init
  (defun cider-cljs-root-dirs ()
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

  (defun cider-conn-type-for-cljc-buffer (&optional buf)
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

  (defun cider-switch-to-releated-repl-buffer (&optional set-namespace)
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

  (defun cider-switch-to-last-clj-buf ()
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
          (when (-some->> (helm-buffer-list)
                          (--filter (with-current-buffer it
                                      (or (eq target-mode    major-mode)
                                          (eq 'clojurec-mode major-mode))))
                          (--sort (let ((it    (or (buf-visit-time it    'all) 0))
                                        (other (or (buf-visit-time other 'all) 0)))
                                    (time-less-p other it)))
                          (-first-item)
                          (funcall display-fn))
            (setq target-modes nil))))))

  (defun cider-add-cur-buf-into-persp (&rest _)
    "TODO"
    (unless (string-equal persp-nil-name (persp-current-name))
      (persp-add-buffer (current-buffer))))

  (defun cider-expected-ns-for-cljs (path)
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

  (defun cider-find-var-at-point ()
    "TODO"
    (interactive)
    (if (cider-connected-p)
        (cider-find-var nil (cider-symbol-at-point))
      (dumb-jump-go)))

  (defun cider-doc-at-point ()
    "TODO"
    (interactive)
    (cider-doc-lookup (cider-symbol-at-point)))

  :config
  (setq cider-repl-display-in-current-window t
        cider-repl-use-pretty-printing t
        cider-cljs-lein-repl (concat "(do"
                                     "  (require 'figwheel-sidecar.repl-api)"
                                     "  (figwheel-sidecar.repl-api/start-figwheel!)"
                                     "  (figwheel-sidecar.repl-api/cljs-repl))"))
  (add-hook 'cider-repl-mode-hook #'cider-add-cur-buf-into-persp)
  (advice-add #'cider-find-var :after #'cider-add-cur-buf-into-persp)
  (advice-add #'cider-connection-type-for-buffer :before-until #'cider-conn-type-for-cljc-buffer)
  (advice-add #'cider-switch-to-repl-buffer :override #'cider-switch-to-releated-repl-buffer)
  (advice-add #'cider-switch-to-last-clojure-buffer :override #'cider-switch-to-last-clj-buf)
  (advice-add #'cider-expected-ns :around #'cider-expected-ns-for-cljs)
  (add-hook 'cider-connected-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point))))

(use-package cider-mode
  :defer t
  :config
  (setq cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil
        cider-mode-line '(:eval (when (cider-connected-p)
                                  "Ⓡ" ; (R)ELP
                                  )))
  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (setq-local font-lock-fontify-region-function
                          #'font-lock-default-fontify-region)
              (eldoc-mode 1))))

(use-package cider-repl-mode
  :defer t
  :config
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (eldoc-mode 1))))

(use-package clojure-mode
  :ensure t
  :defer t
  :init
  (defun clojure-skip (direction-or-item &rest items)
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

  (defun clojure-correct-font-lock-syntatic-face (res)
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

  (defvar context-fn-prefix-regex (regexp-opt '("eval" "run" "exe" "execute"))
    "TODO")

  (defvar context-fn-regex (regexp-opt '("in" "on" "with"))
    "TODO")

  (defun clojure--get-indentation (sym)
    "TODO"
    (when (or (let ((case-fold-search nil))
                (string-match-p "^[-_A-Z]+\\>" sym))
              (string-match-p (concat "^\\("
                                      context-fn-prefix-regex "-\\)?"
                                      context-fn-regex
                                      "\\>")
                              sym))
      :defn))

  (defconst clojure-namespace-name-regex-1
    (rx line-start "("
        (zero-or-one (zero-or-one "clojure.") "core/")
        (zero-or-one "in-") "ns" (zero-or-one "+")
        (one-or-more (any whitespace "\n")))
    "TODO")

  (defun clojure-find-ns-custom ()
    "Sometimes `clojure-find-ns' is slow due to repeatedly call `up-list' to find the top level.
This is customized version of `clojure-find-ns' to improve some performance."
    (save-excursion
      (save-restriction
        (widen)
        (let ((limit (line-end-position))
              matched?
              fancy-narrow--beginning
              fancy-narrow--end)
          (goto-char (point-min))
          (while (re-search-forward clojure-namespace-name-regex-1 limit t)
            (setq matched? t))
          (when matched?
            (goto-char (match-beginning 0))
            (when (re-search-forward clojure-namespace-name-regex nil t)
              (match-string-no-properties 4)))))))

  (defun clojure-setup ()
    "TODO"
    (setq-local font-lock-extend-region-functions
                (remove 'clojure-font-lock-extend-region-def
                        font-lock-extend-region-functions)))

  :config
  (setq clojure-get-indent-function #'clojure--get-indentation)
  (add-hook 'clojure-mode-hook #'clojure-setup :append)
  (advice-add #'clojure-find-ns :override #'clojure-find-ns-custom)
  (advice-add #'clojure-font-lock-syntactic-face-function :filter-return
              #'clojure-correct-font-lock-syntatic-face))

(use-package edn
  :ensure t
  :defer t)

(use-package helm-cider-cheatsheet
  :ensure helm-cider
  :defer t)

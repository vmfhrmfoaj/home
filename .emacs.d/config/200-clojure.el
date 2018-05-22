(use-package cider
  :ensure t
  :defer t
  :commands (cider-connect)
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

  (defun clojure-forward-symbol (n)
    (let ((sym     (concat "^/" clojure--sym-forbidden-rest-chars))
          (not-sym (concat "/"  clojure--sym-forbidden-rest-chars))
          (skip-chars (if (< 0 n)
                          (lambda (s)
                            (skip-chars-forward s)
                            (skip-chars-backward "."))
                        (lambda (s)
                          (skip-chars-backward s)
                          (if (looking-at-p "\\.-?")
                              (skip-chars-forward ".-")))))
          (n (abs n)))
      (while (<= 1 n)
        (setq n (1- n))
        (funcall skip-chars not-sym)
        (funcall skip-chars sym))))

  (defcustom clojure-clj-test-declaration "[clojure.test :refer :all]"
    "TODO"
    :type 'stringp
    :safe 'stringp)

  (defcustom clojure-cljs-test-declaration "[cljs.test :refer-macros [async deftest is testing]]"
    "TODO"
    :type 'stringp
    :safe 'stringp)

  (defcustom clojure-lein-profile-kw ":dev"
    "TODO"
    :type 'stringp
    :safe (lambda (kw)
            (and (stringp kw)
                 (string-match-p "^:" kw))))

  (defun clojure-insert-namespace ()
    (let ((cb
           (lexical-let ((cur-buf (current-buffer))
                         (file-rel-path (clojure-project-relative-path (buffer-file-name))))
             (lambda (resp)
               (let* ((paths (edn-read (nrepl-dict-get resp "value")))
                      (src-dir (->> paths
                                    (--map (if (string-match-p "/$" it)
                                               it
                                             (concat it "/")))
                                    (--filter (string-match-p (concat "^" it) file-rel-path))
                                    (-first-item))))
                 (when src-dir
                   (let ((ns (->> (file-relative-name file-rel-path src-dir)
                                  (file-name-sans-extension)
                                  (replace-regexp-in-string "/+" ".")
                                  (replace-regexp-in-string "_" "-"))))
                     (with-current-buffer cur-buf
                       (goto-char (point-min))
                       (insert "(ns " ns)
                       (when (string-match-p "-test$" ns)
                         (insert "\n")
                         (insert "(:require [" (s-left -5 ns) " :as target]")
                         (let* ((ext (file-name-extension file-rel-path))
                                (clj? (string-match-p "^cljc?$" ext))
                                (cljs? (string-match-p "^clj[cs]$" ext))
                                (both? (string-equal "cljc" ext)))
                           (insert "\n")
                           (when both?
                             (insert "#?@("))
                           (when clj?
                             (when both?
                               (insert ":clj  ["))
                             (insert clojure-clj-test-declaration)
                             (when both?
                               (insert "]")))
                           (when cljs?
                             (when both?
                               (insert "\n")
                               (insert ":cljs ["))
                             (insert clojure-cljs-test-declaration)
                             (when both?
                               (insert "]")))
                           (when both?
                             (insert ")")))
                         (insert ")"))
                       (insert ")\n\n")
                       (indent-region (point-min) (point-max))))))))))
      (if (string-equal "cljs" (cider-connection-type-for-buffer))
          (cider-tooling-eval
           (concat "(let [builds (->> \"project.clj\"
                                      (slurp)
                                      (clojure.edn/read-string)
                                      (drop 3)
                                      (apply hash-map)
                                      :cljsbuild :builds)]
                      (:source-paths (cond
                                       (map? builds) (get builds " clojure-lein-profile-kw ")
                                       (sequential? builds) (first (filter #(= (name " clojure-lein-profile-kw ") (:id %)) builds))
                                       :else nil)))") cb)
        (cider-tooling-eval
         (concat "(let [proj (->> \"project.clj\"
                                   (slurp)
                                   (clojure.edn/read-string)
                                   (drop 3)
                                   (apply hash-map))]
                    (concat (get-in proj [:source-paths])
                            (get-in proj [:profiles " clojure-lein-profile-kw " :source-paths])))") cb))))

  (defun clojure-font-lock-syntactic-face-function-2 (res)
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

  :config
  (advice-add #'clojure-font-lock-syntactic-face-function :before
              #'clojure-font-lock-syntactic-face-function-2)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local custom-forward-symbol #'clojure-forward-symbol)
              (setq-local font-lock-extend-region-functions
                          (remove 'clojure-font-lock-extend-region-def
                                  font-lock-extend-region-functions))
              (when (and buffer-file-name
                         (= (point-min) (point-max))
                         (cider-connected-p))
                (clojure-insert-namespace))

              (let ((file-name (or (buffer-file-name) ""))
                    (ns-form (save-match-data
                               (save-excursion
                                 (if (clojure-find-ns)
                                     (progn
                                       (goto-char (match-beginning 0))
                                       (end-of-defun)
                                       (let ((end (point)))
                                         (backward-sexp 1)
                                         (buffer-substring-no-properties (point) end)))
                                   ""))))
                    (compojure-kws '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH" "ANY" "context"))
                    keywords)
                (cond
                 ((string-match-p "_test.clj[cs]?$" file-name)
                  (add-to-list 'keywords "async")
                  (add-to-list 'keywords "is"))
                 ((string-match-p (concat "compojure.core :"
                                          "\\(?:as"
                                          "\\|refer[ \r\t\n]\\[[^]]*"
                                          (regexp-opt compojure-kws)
                                          "\\>\\)")
                                  ns-form)
                  (setq keywords (append keywords compojure-kws))))
                (when keywords
                  (setq-local clojure-get-indent-function
                              (lexical-let ((keywords (regexp-opt keywords)))
                                (byte-compile
                                 (lambda (func-name)
                                   (and (string-match-p keywords func-name) :defn))))))))
            :append))

(use-package edn
  :ensure t)

(defun cider-cljs-root-dirs ()
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

(defun cider-connection-type-for-cljc-buffer ()
  (when (eq 'clojurec-mode major-mode)
    (let ((cur-pos (point)))
      (save-excursion
        (when (re-search-backward "#\\?@?(" nil t)
          (let ((beg-pos (prog1 (point) (forward-sexp)))
                (end-pos (point)))
            (when (< beg-pos cur-pos end-pos)
              (goto-char cur-pos)
              (when (re-search-backward "\\(?:^\\s-*\\|(\\):\\(cljs?\\)[ \r\t\n]+"  beg-pos t)
                (match-string-no-properties 1)))))))))


(defun clojure--binding-regexp ()
  (concat "(" (regexp-opt clojure--binding-forms) "[ \r\t\n]+\\["))

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

(defun clojure-forward-sexp (&optional n)
  (or n (setq n 1))
  (while (not (zerop n))
    (if (> n 0) (clojure-skip  1 :comment :ignored-form :tagged-literal))
    (forward-sexp (if (< 0 n) 1 -1))
    (if (< n 0) (clojure-skip -1 :comment :ignored-form :tagged-literal))
    (setq n (funcall (if (< 0 n) '1- '1+) n))))

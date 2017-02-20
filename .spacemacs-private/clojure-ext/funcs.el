(defun cider-cljs-root-dirs ()
  (let (repl)
    (dolist (buf (cider-connections) repl)
      (with-current-buffer buf
        (when (string= "clj" cider-repl-type)
          (setq repl buf))))
    (when repl
      (ignore-errors
        (-some-> (concat "(let [b (some-> \"project.clj\""
                         "                (slurp)"
                         "                (clojure.edn/read-string)"
                         "                (some->> (drop 3) (apply hash-map))"
                         "                (get-in [:cljsbuild :builds]))"
                         "        b (cond-> b (map? b) (vals))]"
                         "  (some->> b"
                         "           (mapcat :source-paths)"
                         "           (apply hash-set)"
                         "           (seq)"
                         "           (map clojure.java.io/as-file)"
                         "           (map (memfn getAbsolutePath))))")
                 (nrepl-sync-request:eval repl)
                 (nrepl-dict-get "value")
                 (edn-read))))))

(defvar clojure--binding-forms
  '("let" "if-let" "when-let"))

(defvar clojure--binding-regexp
  (concat (regexp-opt clojure--binding-forms) "[ \r\t\n]*\\["))

(defun clojure-space-key ()
  (interactive)
  (if (->> (syntax-ppss)
           (-drop 3)
           (-take 2)
           (--any? (and t it)))
      (insert " ")
    (let ((point     (point))
          (column    (progn (ignore-errors (backward-sexp)) (current-column)))
          (start-pos (progn (backward-up-list 1 t) (point)))
          (end-pos   (progn (forward-sexp) (point))))
      (clojure--goto-let)
      ;; Is the current position inside the let binding?
      (if (prog1 (not (and (re-search-forward clojure--binding-regexp end-pos t)
                           (= column     (current-column))
                           (>= start-pos (progn (backward-char) (point)))
                           (<= end-pos   (progn (forward-list)  (point)))))
            (goto-char point))
          (insert " ")
        (if (>= start-pos (progn
                            (previous-line)
                            (end-of-line)
                            (backward-sexp)
                            (point)))
            (progn
              (goto-char point)
              (insert " "))
          (let ((target-pos    (point))
                (target-column (current-column))
                (cur-column    (progn
                                 (goto-char point)
                                 (current-column))))
            (when (>= cur-column target-column)
              (goto-char target-pos)
              (while (>= cur-column (current-column))
                (setq point (1+ point))
                (insert " "))
              (goto-char point))
            (insert " ")
            (while (let ((column (current-column)))
                     (> target-column column))
              (insert " "))))))))

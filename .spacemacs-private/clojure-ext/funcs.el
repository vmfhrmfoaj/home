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
                         "      b (cond-> b (map? b) (vals))]"
                         "  (some->> b"
                         "           (mapcat :source-paths)"
                         "           (apply hash-set)"
                         "           (seq)"
                         "           (map clojure.java.io/as-file)"
                         "           (map (memfn getAbsolutePath))))")
                 (nrepl-sync-request:eval repl)
                 (nrepl-dict-get "value")
                 (edn-read))))))

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

(defun clojure-space-key ()
  (interactive)
  ;; string or comment?
  (if (->> (syntax-ppss)
           (-drop 3)
           (-take 2)
           (--any? (and t it)))
      (insert " ")
    (let ((point (point)))
      (condition-case nil
          (let ((column    (progn (ignore-errors (backward-sexp)) (current-column)))
                (start-pos (progn (backward-up-list 1 t) (point)))
                (end-pos   (progn (forward-sexp) (point))))
            (ignore-errors
              (while (not (looking-at-p (clojure--binding-regexp)))
                (backward-up-list 1 t)))
            ;; Is the current position inside the let binding?
            (if (prog1 (not (and (re-search-forward (clojure--binding-regexp) end-pos t)
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
                      (prev-column   (progn
                                       (goto-char point)
                                       (previous-line)
                                       (beginning-of-line-text)
                                       (current-column)))
                      (cur-column (progn
                                    (goto-char point)
                                    (current-column))))
                  (when (and (not (< column prev-column))
                             (>= cur-column target-column))
                    (goto-char target-pos)
                    (while (and (>= clojure-spc-key-valign-skip (- cur-column (current-column)))
                                (>= cur-column (current-column)))
                      (setq point (1+ point))
                      (insert " "))
                    (goto-char point))
                  (insert " ")
                  (while (let ((cur-col (current-column)))
                           (and (not (< column prev-column))
                                (>= clojure-spc-key-valign-skip (- target-column cur-col))
                                (> target-column cur-col)))
                    (insert " "))))))
        (error (goto-char point)
               (insert " "))))))

(defun clojure-skip (&rest items)
  (let ((l items))
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
          (forward-sexp)))
       ((eq (car l) :ignored-form)
        (if (not (looking-at-p "#_\\|,"))
            (setq l (cdr l))
          (setq l (-remove-item :ignored-form items))
          (or (/= 0 (skip-chars-forward ","))
              (forward-sexp (if (looking-at-p "#_\\s(") 2 1)))
          (clojure-skip :ignored-form)))
       ((eq (car l) :destructuring-bind)
        (if (not (looking-at-p "{\\|\\["))
            (setq l (cdr l))
          (setq l (-remove-item :destructuring-bind items))
          (forward-sexp)
          (apply #'clojure-skip l)
          (forward-sexp)
          (clojure-skip :destructuring-bind)))
       (t (setq l (cdr l)))))))

(defun clojure-forward-sexp (&optional n)
  (or n (setq n 1))
  (while (not (zerop n))
    (clojure-skip :comment :ignored-form)
    (forward-sexp (if (< 0 n) 1 -1))
    (setq n (funcall (if (< 0 n) '1- '1+) n))))

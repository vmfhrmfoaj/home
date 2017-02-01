(defun isearch-fn (regx &optional display-str)
  (lexical-let ((regx regx)
                (display-str (or display-str regx)))
    (lambda ()
      (interactive)
      (setq isearch-string  (concat isearch-string regx)
            isearch-message (concat isearch-message
                                    (mapconcat #'isearch-text-char-description
                                               display-str ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(defun wrap-sp-forward-symbol (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p wrap-sp-supported-modes)
               (-some->> (buffer-substring (point) (line-end-position))
                         (string-match (concat "^\\s-*"
                                               sp-clojure-prefix
                                               "[^({\\[]"))))
      (goto-char (+ (point) (match-end 0))))))

(defun wrap-sp-backward-symbol (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p wrap-sp-supported-modes)
               (-some->> (buffer-substring (line-beginning-position) (point))
                         (string-match (concat sp-clojure-prefix
                                               "\\s-*$"))))
      (beginning-of-line)
      (goto-char (+ (point) (match-beginning 0))))))

(defun wrap-sp-forward-sexp (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p wrap-sp-supported-modes)
               (-some->> (char-after)
                         (char-to-string)
                         (string-match "\\s(")))
      (forward-sexp))))

(defun wrap-sp-backward-sexp (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p wrap-sp-supported-modes)
               (-some->> (buffer-substring (line-beginning-position) (point))
                         (string-match (concat sp-clojure-prefix "\\s-*$"))))
      (beginning-of-line)
      (goto-char (+ (point) (match-beginning 0))))))

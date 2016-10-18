(setq-default auto-indent-skip-when-open-file t)

(defun auto-indent (&rest _)
  "auto-indent-for-evil-mode"
  (unless auto-indent-skip-when-open-file
    (save-match-data
      (save-mark-and-excursion
       (when (derived-mode-p 'prog-mode)
         (let ((start (progn
                        (beginning-of-defun)
                        (point)))
               (end   (progn
                        (end-of-defun)
                        (point))))
           (indent-region start end))))))
  (setq-local auto-indent-skip-when-open-file nil))

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

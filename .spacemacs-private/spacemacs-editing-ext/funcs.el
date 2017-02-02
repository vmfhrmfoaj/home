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

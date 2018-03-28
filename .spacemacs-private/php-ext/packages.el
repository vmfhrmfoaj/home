;;; packages.el --- php-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst php-ext-packages
  '(php-mode))

(defun php-ext/post-init-php-mode ()
  (use-package php-mode
    :defer t
    :config
    (font-lock-add-keywords
     'php-mode
     (let* ((symbol "\\$[_0-9a-zA-Z]+")
            (whitespace "[ \r\t\n]")
            (whitespace+ (concat whitespace "+"))
            (whitespace* (concat whitespace "*"))
            (assigment (concat whitespace* "[.+-*/]?=[^=>]")))
       `((,(concat "\\(" symbol "\\|->[_0-9a-zA-Z]+\\)")
          (1 (cond
              ((sp-point-in-string)  'font-lock-string-face)
              ((sp-point-in-comment) 'font-lock-comment-face)
              (t nil))
             t))
         (,(concat "\\(" symbol "\\)\\(\\[[^]]*\\]\\)*" assigment)
          (1 'font-lock-variable-name-face))
         (,(concat symbol "->\\([_0-9a-zA-Z]+\\)\\(\\[[^]]*\\]\\)*" assigment)
          (1 'font-lock-variable-name-face))
         (,(concat "list(\\(" "\\(\"[_0-9A-Za-z]+\"" whitespace* "=>" whitespace* "\\)?" symbol whitespace* ",?" whitespace* "\\)+)" assigment)
          (,(concat "\\(" symbol "\\)")
           (progn
             (goto-char (match-beginning 0))
             (safe-down-list-1)
             (save-excursion
               (safe-up-list-1)
               (point)))
           nil
           (1 'font-lock-variable-name-face)))))
     'append)))

;;; packages.el ends here

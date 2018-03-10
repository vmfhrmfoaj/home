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
          (1 (let* ((pos (if (eolp) (1- (match-beginning 0)) (1+ (match-end 0))))
                    (face (plist-get (text-properties-at pos) 'face))
                    (face-lst (if (listp face) face (list face))))
               (when (or (memq 'font-lock-comment-face           face-lst)
                         (memq 'font-lock-comment-delimiter-face face-lst)
                         (memq 'font-lock-string-face            face-lst))
                 face))
             t))
         (,(concat "\\(" symbol "\\)\\(\\[[^]]*\\]\\)?" assigment)
          (1 'font-lock-variable-name-face))
         (,(concat symbol "->\\([_0-9a-zA-Z]\\)\\(\\[[^]]*\\]\\)?" assigment)
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

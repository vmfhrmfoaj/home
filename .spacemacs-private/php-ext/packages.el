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
     '(("\\(\\$[_0-9a-zA-Z]+\\)"
        (1 (let ((face (plist-get (text-properties-at (1- (match-beginning 0))) 'face)) face-lst)
             (setq face-lst (if (listp face) face (list face)))
             (when (or (memq 'font-lock-comment-face face-lst)
                       (memq 'font-lock-string-face  face-lst))
               face))
           t))
       ("\\(\\$[_0-9a-zA-Z]+\\)\\s-*[.+-*/]?=[^=]"
        (1 'font-lock-variable-name-face))
       ("(\\(\\(?:\\$[_0-9a-zA-Z]+\\(?:,\\s-*\\)?\\)+\\))\\s-*[.+-*/]?=[^=]"
        (1 'font-lock-variable-name-face)))
     'append)))

;;; packages.el ends here

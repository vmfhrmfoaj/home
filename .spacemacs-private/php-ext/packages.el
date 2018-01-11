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
     '(("\\(\\$[_0-9a-zA-Z]+\\)\\_>"
        (1 'default t))
       ("\\(\\$[_0-9a-zA-Z]+\\)\\s-*[.+-*/]?=[^~]"
        (1 'font-lock-variable-name-face t))
       ("(\\(\\(?:\\$[_0-9a-zA-Z]+\\(?:,\\s-*\\)?\\)+\\))\\s-*[.+-*/]?=[^~]"
        (1 'font-lock-variable-name-face t)))
     'append)))

;;; packages.el ends here

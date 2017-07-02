;;; packages.el --- spacemacs-ui-visual-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst spacemacs-ui-visual-ext-packages
  '(hl-todo))

(defun spacemacs-ui-visual-ext/post-init-hl-todo ()
  (use-package hl-todo
    :defer t
    :config
    (setq hl-todo-keywords
          (list (list (caar hl-todo-keywords)
                      `(1 (hl-todo-get-face) prepend))))
    (advice-add #'hl-todo-get-face :filter-return
                (byte-compile
                 (lambda (ret)
                   (list ret))))))

;;; packages.el ends here

;;; packages.el --- spacemacs-evil-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst spacemacs-evil-ext-packages
  '(linum-relative))

(defun spacemacs-evil-ext/post-init-linum-relative ()
  (use-package linum-relative
    :config
    (add-hook 'prog-mode-hook (-partial #'linum-relative-mode 1))
    (add-hook 'linum-relative-mode-hook (-partial #'diminish 'linum-relative-mode))
    (let ((height (face-attribute 'default :height)))
      (custom-set-faces
       `(linum ((t :underline nil :height ,height)))
       `(linum-relative-current-face ((t :underline nil :height ,height)))))))

;;; packages.el ends here

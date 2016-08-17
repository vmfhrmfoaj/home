;;; packages.el --- focus layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst focus-packages
  '(focus))

(defun focus/init-focus ()
  (eval-after-load 'evil
    '(use-package focus
       :ensure t
       :init
       (add-hook 'evil-insert-state-entry-hook (lambda () (focus-mode 1)))
       (add-hook 'evil-insert-state-exit-hook  (lambda () (focus-mode 0)))
       :config
       (add-to-list 'focus-mode-to-thing '(tex-mode . page))
       (add-to-list 'focus-mode-to-thing '(org-mode . org))
       (put 'org 'bounds-of-thing-at-point
            (lambda ()
              (save-excursion
                (let ((start (progn
                               (outline-previous-heading)
                               (point)))
                      (end   (progn
                               (outline-next-visible-heading 1)
                               (beginning-of-line)
                               (point))))
                  (cons start end))))))))

;;; packages.el ends here

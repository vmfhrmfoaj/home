;;; packages.el --- auto-completion-ext layer packages file for Spacemacs.
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

(defconst auto-completion-ext-packages
  '(company
    helm-company))

(defun auto-completion-ext/post-init-company ()
  (use-package company
    :defer t
    :config
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-s") #'helm-company)
    (with-eval-after-load "evil"
      (evil-global-set-key 'insert (kbd "C-i") #'helm-company))
    (setq company-idle-delay nil)
    (global-company-mode 1)))

(defun auto-completion-ext/post-init-helm-company ()
  (use-package helm-company
    :after helm
    :if (configuration-layer/package-usedp 'company)
    ;; Load package
    ))

;;; packages.el ends here

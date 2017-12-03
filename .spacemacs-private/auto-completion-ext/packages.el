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
    (byte-compile #'helm-company-action-insert-plus)
    (byte-compile #'helm-company-plus)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-s")  #'helm-company)
    (setq company-idle-delay 0.2)))

(defun auto-completion-ext/post-init-helm-company ()
  (use-package helm-company
    :if (configuration-layer/package-usedp 'company)
    ;; Load package
    ))

;;; packages.el ends here

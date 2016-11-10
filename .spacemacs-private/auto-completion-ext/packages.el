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
  '(company))

(defun auto-completion-ext/post-init-company ()
  (use-package company
    :defer t
    :config
    (global-set-key (kbd "<S-tab>") #'completion-at-point)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-s") #'completion-at-point)
    (setq tab-always-indent t           ; TAB do not have intelligent behavior.
          company-idle-delay 0.1)))

;;; packages.el ends here

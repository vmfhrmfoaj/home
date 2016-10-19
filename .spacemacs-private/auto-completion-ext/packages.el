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
  auto-completion-packages)

(defun auto-completion-ext/post-init-company ()
  (use-package company
    :defer t
    :config
    (global-set-key (kbd "<S-tab>") #'completion-at-point)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-s") #'completion-at-point)
    (setq tab-always-indent t           ; TAB do not have intelligent behavior.
          company-tooltip-exclude-modes '(prettify-symbols-mode)
          company-tooltip-exclude-mode-status nil)
    (advice-add #'company-call-frontends :before
                (lambda (cmd)
                  (cond
                   ((eq 'show cmd)
                    (setq-local company-tooltip-exclude-mode-status
                                (-map #'symbol-value company-tooltip-exclude-modes))
                    (disable-modes company-tooltip-exclude-modes))
                   ((eq 'hide cmd)
                    (resotre-modes company-tooltip-exclude-modes
                                   company-tooltip-exclude-mode-status))))))
  )

;;; packages.el ends here

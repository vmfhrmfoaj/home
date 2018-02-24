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
    (define-key company-active-map (kbd "C-s")  #'helm-company)
    (setq company-idle-delay 0.2)
    (global-company-mode)))

(defun auto-completion-ext/post-init-helm-company ()
  (use-package helm-company
    :if (configuration-layer/package-usedp 'company)
    :config
    ;; NOTE:
    ;; Releated issues:
    ;;  - https://github.com/emacs-helm/helm/issues/1971
    ;;  - https://github.com/emacs-helm/helm/issues/1973
    ;; Commit:
    ;;  - https://github.com/emacs-helm/helm/commit/67d149dd5c2b01ec2db8c8be9a6c3809348a9a98
    ;; Backtrace:
    ;;  > (wrong-type-argument window-live-p nil)
    ;;  > #<subr select-window>(nil norecord)
    ;;  > select-window(nil norecord)
    ;;  > helm--autoresize-hook()
    ;;  > run-hooks(helm-after-update-hook)
    ;;  > helm-log-run-hook(helm-after-update-hook)
    ;;  > #[0 ...]
    ;;  > helm-update(nil)
    ;;  > helm-read-pattern-maybe("Pattern: " "ac " nil noresume nil nil nil)
    ;;
    ;; It's workaround.
    (advice-add #'completion-at-point :override #'helm-company)))

;;; packages.el ends here

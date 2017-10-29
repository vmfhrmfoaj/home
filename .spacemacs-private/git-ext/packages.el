;;; packages.el --- git-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: KimJinseop <Jinseop@KimJinseops-iMac.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst git-ext-packages
  '(magit
    magit-svn))

(defun git-ext/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (setq magit-diff-refine-hunk t)
    (-update->> magit-status-sections-hook
                (-replace-first 'magit-insert-unpushed-to-upstream-or-recent
                                'magit-insert-unpushed-to-upstream))
    (if dotspacemacs-fullscreen-at-startup
        (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
    (add-hook 'magit-revision-mode-hook (lambda () (setq-local line-spacing 0))))

  (use-package magit-blame
    :defer
    :config
    (define-key magit-blame-mode-map "n" nil)
    (define-key magit-blame-mode-map "N" nil)))

(defun git-ext/init-magit-svn ()
  (use-package magit-svn
    :if git-enable-magit-svn-plugin
    :config
    (add-hook 'magit-mode-hook 'magit-svn-mode)
    (when (featurep 'evil-magit)
      (evil-magit-define-key 'normal 'magit-mode-map "~" 'magit-svn-popup))))

;;; packages.el ends here

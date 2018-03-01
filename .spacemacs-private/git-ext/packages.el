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
    magit-svn
    smartparens))

(defun git-ext/post-init-smartparens ()
  (add-hook 'git-commit-mode-hook #'smartparens-mode))

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
    (add-hook 'magit-status-mode-hook #'persp-add-buffer)
    (add-hook 'magit-revision-mode-hook (lambda () (setq-local line-spacing 0)))
    (advice-add #'magit-log-propertize-keywords :filter-return
                (byte-compile
                 (lambda (msg)
                   (let ((type  "[-_/A-Za-z]+")
                         (scope "[-_/A-Za-z, ]+"))
                     (when (and magit-log-highlight-keywords
                                (string-match (concat "^\\(" type "\\)\\(?:(\\(" scope "\\))\\)?:") msg))
                       (put-text-property (match-beginning 1)
                                          (match-end 1)
                                          'face 'magit-commit-log-type-face
                                          msg)
                       (when (match-beginning 2)
                         (put-text-property (match-beginning 2)
                                            (match-end 2)
                                            'face 'magit-commit-log-scope-face
                                            msg))))
                   msg))))

  (use-package magit-blame
    :defer t
    :config
    (define-key magit-blame-mode-map "n" nil)
    (define-key magit-blame-mode-map "N" nil)))

(defun git-ext/init-magit-svn ()
  (use-package magit-svn
    :if git-enable-magit-svn-plugin
    :after magit
    :diminish magit-svn-mode
    :config
    (add-hook 'magit-mode-hook 'magit-svn-mode)
    (when (featurep 'evil-magit)
      (evil-magit-define-key 'normal 'magit-mode-map "~" 'magit-svn-popup))))

;;; packages.el ends here

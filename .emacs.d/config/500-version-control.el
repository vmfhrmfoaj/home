;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'git-timemachine nil t)
  (require 'git-gutter-fringe nil t)
  (require 'gitignore-mode nil t)
  (require 'magit nil t)
  (require 'magit-svn nil t))

(use-package git-timemachine
  :ensure t
  :defer t
  :config
  (defun git-timemachine--custom-blame ()
    "Call ‘magit-blame’ on current revision."
    (interactive)
    (if (fboundp 'magit-blame-addition)
        (let ((magit-buffer-revision (car git-timemachine-revision)))
          (magit-blame-addition nil))
      (message "You need to install magit for blame capabilities")))

  (advice-add #'git-timemachine-blame :override #'git-timemachine--custom-blame))

(use-package git-gutter-fringe
  :ensure t
  :defer t
  :init
  (defun git-gutter-fringe-setup ()
    (let* ((file (buffer-file-name))
           (file-name (file-name-nondirectory file)))
      (remove-hook 'find-file-hook #'git-gutter-fringe-setup)
      (require 'git-gutter-fringe)
      (global-git-gutter-mode t)))

  (add-hook 'find-file-hook #'git-gutter-fringe-setup)

  :config
  (when window-system
    (let* ((max (cond
                 ((numberp fringe-mode) fringe-mode)
                 ((consp fringe-mode) (car fringe-mode))
                 (t 8)))
           (padding 2)
           (width (- max (* 2 padding)))
           (bitmap (-repeat (+ 2 (line-pixel-height) line-spacing)
                            (apply #'concat (append (-repeat padding ".")
                                                    (-repeat width   "X")
                                                    (-repeat padding ".")))))
           (fr-vec (apply #'fringe-helper-convert bitmap)))
      (define-fringe-bitmap 'git-gutter-fr:added    fr-vec nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr:deleted  fr-vec nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr:modified fr-vec nil nil nil)))

  (defvar git-gutter:post-command-timer nil)

  (advice-add #'git-gutter:post-command-hook :around
              (lambda (fn &rest args)
                "To avoid frequently run in the short time."
                (when git-gutter:post-command-timer
                  (cancel-timer git-gutter:post-command-timer))
                (setq git-gutter:post-command-timer
                      (run-at-time 0.25 nil
                                   (lambda ()
                                     (setq git-gutter:post-command-timer nil)
                                     (apply fn args)))))))

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :commands (magit-unstaged-files)
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  (defface magit-commit-log-type-face
    `((t (:inherit font-lock-function-name-face :weight ,(face-attribute 'default :weight))))
    "TODO")

  (defface magit-commit-log-scope-face
    `((t (:inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
    "TODO")

  :config
  (defun magit-log-propertize-keywords-for-conventional-commits (msg)
    "TODO"
    (ignore-errors
      (let ((type  "[^:() ]+")
            (scope "[^)]+"))
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
                               msg)))))
    msg)

  (setq magit-bury-buffer-function #'magit-mode-quit-window
        magit-diff-refine-hunk t
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  (-update->> magit-status-sections-hook
              (-replace-first 'magit-insert-unpushed-to-upstream-or-recent
                              'magit-insert-unpushed-to-upstream))

  ;; NOTE
  ;;  The result of `(call-process "env" nil t t)` does not contain 'HOME' variable.
  ;;  But on Linux or without the configuration(e.g. emacs -Q), it is ok.
  ;;  So, I think it is a bug caused by collision between Spacemacs and latest Emacs-macport.
  ;; FIXME
  ;;  This is workaround.
  ;; (add-to-list 'magit-git-environment (concat "HOME=" home-dir))
  ;; (add-to-list 'magit-git-environment (concat "SSH_AUTH_SOCK=" (getenv "SSH_AUTH_SOCK")))
  (advice-add #'magit-log-propertize-keywords :filter-return
              #'magit-log-propertize-keywords-for-conventional-commits))

(use-package magit-svn
  :ensure t
  :after magit
  :config
  (add-hook 'magit-mode-hook 'magit-svn-mode)

  (let ((f (lambda ()
             (ignore-errors
               (string-match-p "^\\(master\\|svn\\)$" (magit-rev-branch "HEAD"))))))
    (advice-add #'magit-insert-svn-unpulled :before-while f)
    (advice-add #'magit-insert-svn-unpushed :before-while f)))

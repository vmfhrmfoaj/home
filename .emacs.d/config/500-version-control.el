(use-package evil-magit
  :ensure t
  :after magit)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-gutter-fringe+
  :ensure t
  :defer t
  :init
  (defun git-gutter-fringe+-setup ()
    (remove-hook 'find-file-hook #'git-gutter-fringe+-setup)
    (require 'git-gutter-fringe+)
    (git-gutter+-mode 1))

  (add-hook 'find-file-hook #'git-gutter-fringe+-setup)

  :config
  (setq git-gutter+-disabled-modes '(org-mode)
        git-gutter-fr+-side 'left-fringe)
  (let* ((max (cond
               ((numberp fringe-mode) fringe-mode)
               ((consp fringe-mode) (car fringe-mode))
               (t 8)))
         (padding 2)
         (width (- max (* 2 padding)))
         (bitmap (-repeat (+ (1- (line-pixel-height)) line-spacing)
                          (apply #'concat (append (-repeat padding ".")
                                                  (-repeat width   "X")
                                                  (-repeat padding ".")))))
         (fr-vec (apply #'fringe-helper-convert bitmap)))
    (define-fringe-bitmap 'git-gutter-fr+-added    fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-deleted  fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-modified fr-vec nil nil nil))
  (global-git-gutter+-mode 1))

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :init
  (defface magit-commit-log-type-face
    `((t (:inherit font-lock-function-name-face :weight normal)))
    "TODO")

  (defface magit-commit-log-scope-face
    `((t (:inherit font-lock-variable-name-face :weight normal)))
    "TODO")

  (defun magit-log-propertize-keywords-for-conventional-commits (msg)
    "TODO"
    (let ((type  "[^:()]+")
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
                             msg))))
    msg)

  (defun magit-setup ()
    (remove-hook 'magit-mode-hook #'magit-setup)
    (require 'helm nil t))

  (add-hook 'magit-mode-hook #'magit-setup)

  :config
  (setq magit-diff-refine-hunk t)
  (-update->> magit-status-sections-hook
              (-replace-first 'magit-insert-unpushed-to-upstream-or-recent
                              'magit-insert-unpushed-to-upstream))
  ;; NOTE
  ;;  The result of `(call-process "env" nil t t)` does not contain 'HOME' variable.
  ;;  But on Linux or without the configuration(e.g. emacs -Q), it is ok.
  ;;  So, I think it is a bug caused by collision between Spacemacs and latest Emacs-macport.
  ;; FIXME
  ;;  This is workaround.
  ;; (add-to-list 'magit-git-environment (concat "HOME=" (getenv "HOME")))
  ;; (add-to-list 'magit-git-environment (concat "SSH_AUTH_SOCK=" (getenv "SSH_AUTH_SOCK")))
  (advice-add #'magit-log-propertize-keywords :filter-return
              #'magit-log-propertize-keywords-for-conventional-commits))

(use-package magit-svn
  :ensure t
  :after magit
  :config
  (add-hook 'magit-mode-hook 'magit-svn-mode))

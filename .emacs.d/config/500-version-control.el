(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (setq git-gutter+-disabled-modes '(org-mode)
        git-gutter-fr+-side 'left-fringe)
  (let* ((max 8)
         (prefix 2)
         (width 5)
         (width (min width (- max prefix)))
         (postfix (- max prefix width))
         (bitmap (-repeat (+ (1- (line-pixel-height)) line-spacing)
                          (apply #'concat (append (-repeat prefix  ".")
                                                  (-repeat width   "X")
                                                  (-repeat postfix ".")))))
         (fr-vec (apply #'fringe-helper-convert bitmap)))
    (define-fringe-bitmap 'git-gutter-fr+-added    fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-deleted  fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-modified fr-vec nil nil nil))
  (add-hook 'find-file-hook #'git-gutter+-mode))

(use-package evil-magit
  :ensure t
  :after magit)

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status)
  :config
  ;; NOTE
  ;;  The result of `(call-process "env" nil t t)` does not contain 'HOME' variable.
  ;;  But on Linux or without the configuration(e.g. emacs -Q), it is ok.
  ;;  So, I think it is a bug caused by collision between Spacemacs and latest Emacs-macport.
  ;; FIXME
  ;;  This is workaround.
  (add-to-list 'magit-git-environment (concat "HOME=" (getenv "HOME"))))

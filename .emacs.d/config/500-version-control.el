(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package git-gutter-fringe+
  :ensure t
  :config
  (setq git-gutter+-disabled-modes '(org-mode)
        git-gutter-fr+-side 'left-fringe)
  (let* ((max     8)
         (prefix  2)
         (width   5)
         (width   (min width (- max prefix)))
         (postfix (- max prefix width))
         (bitmap  (-repeat (+ (1- (line-pixel-height)) line-spacing)
                           (apply #'concat (append (-repeat prefix  ".")
                                                   (-repeat width   "X")
                                                   (-repeat postfix ".")))))
         (fr-vec (apply #'fringe-helper-convert bitmap)))
    (define-fringe-bitmap 'git-gutter-fr+-added    fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-deleted  fr-vec nil nil nil)
    (define-fringe-bitmap 'git-gutter-fr+-modified fr-vec nil nil nil)

    (add-hook 'find-file-hook #'git-gutter+-mode)))

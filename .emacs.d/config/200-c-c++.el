(use-package cc-mode
  :defer t
  :config
  (defn man-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (man (concat "--sections=2 3 " thing))))

  (setq Man-notify-method 'aggressive
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))

  (setq-default c-backslash-column 80)
  (setq-default c-backslash-max-column 120)

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'case-label '+)
              (unless (and (fboundp #'editorconfig-core-get-nearest-editorconfig)
                           (editorconfig-core-get-nearest-editorconfig default-directory))
                (setq c-basic-offset 4))
              (when (member major-mode '(c-mode c++-mode))
                (setq-local evil-lookup-func #'man-at-point)
                (make-local-variable 'font-lock-extend-region-functions)
                (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
                (c-toggle-auto-newline -1)))
            :append)

  (add-hook 'java-mode-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'case-label '+)
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point))
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines))))

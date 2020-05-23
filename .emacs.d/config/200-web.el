(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package js
  :defer t
  :config
  (setq-default js-indent-level 4)
  (add-hook 'js-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package js2-mode
  :disabled t
  :ensure t
  :defer t
  :mode (("\\.jsm?\\'" . js2-mode)
         ("\\.json\\'" . js2-mode)
         ("\\.jsx\\'"  . js2-jsx-mode))

  :config
  (defconst js--prettify-symbols-alist nil)
  (setq-default js-indent-level 4
                js2-basic-offset 2)
  (add-hook 'js2-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package mmm-mode
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'mmm-mode)
    (quelpa '(mmm-mode :fetcher github :repo "purcell/mmm-mode"))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package vue-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html\\.eex\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (with-eval-after-load "smartparens"
    (add-hook 'web-mode-hook
              (lambda ()
                (-update->> web-mode-auto-pairs
                            (--map (let ((a (car it))
                                        (b (substring (cdr it) 0 -1)))
                                    `(,a . ,b))))))))

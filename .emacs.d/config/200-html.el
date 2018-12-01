(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package web-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\.eex\\'" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex$\\'")))

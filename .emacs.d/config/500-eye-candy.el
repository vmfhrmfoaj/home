(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

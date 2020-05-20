(use-package creole-mode
  :ensure t
  :defer t
  :mode "\\.wikicreole\\'")

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

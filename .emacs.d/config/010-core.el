(use-package evil
  :ensure t
  :config
  (setq-default evil-symbol-word-search 'thing-at-point)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1))

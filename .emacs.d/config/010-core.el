(use-package evil
  :ensure t
  :config
  (setq-default evil-symbol-word-search 'thing-at-point)
  (global-subword-mode 1)
  (evil-mode 1))

(use-package helm-config
  :ensure helm
  :config
  (setq helm-autoresize-min-height 25
        helm-autoresize-max-height 45)
  (helm-mode 1)
  (helm-autoresize-mode 1))

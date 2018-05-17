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
  :defer t
  :commands (helm-M-x
             helm-buffers-list
             helm-find-files
             helm-projectile-find-dir
             helm-resume)
  :config
  (setq helm-always-two-windows t)
  (helm-mode 1))

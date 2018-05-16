(use-package helm-config
  :ensure helm
  :config
  (setq helm-always-two-windows t)
  (helm-mode 1))

(use-package helm-company
  :ensure t
  :after company
  :config
  (define-key company-active-map (kbd "C-s") #'helm-company))

(use-package helm-projectile
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons))

(use-package helm-config
  :ensure helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
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

(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-find-dir
             helm-projectile-find-file
             helm-projectile-switch-project))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons))

(use-package dumb-jump
  :ensure t)

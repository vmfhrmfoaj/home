(use-package projectile
  :ensure t
  :diminish ""
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

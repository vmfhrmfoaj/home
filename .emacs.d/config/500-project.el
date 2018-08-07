(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t
        projectile-mode-line "")
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

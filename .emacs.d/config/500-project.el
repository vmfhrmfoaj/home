(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm
        projectile-mode-line "Ⓟ")
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

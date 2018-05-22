(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm
        projectile-mode-line "Ⓟ")
  (add-hook 'after-init-hook
            (lambda ()
              (projectile-load-known-projects)
              (projectile-cleanup-known-projects)))
  (projectile-mode 1))

(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :commands (projectile-load-known-projects)
  :init
  (projectile-load-known-projects)

  :config
  (advice-add #'projectile-project-root :before-until (lambda (&optional _) (persp-current-project)))
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

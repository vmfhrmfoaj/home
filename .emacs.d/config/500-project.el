(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :config
  (advice-add #'projectile-project-root :before-until (lambda (&optional _) (persp-current-project)))
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

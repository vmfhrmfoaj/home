(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :commands (projectile-load-known-projects)
  :init
  (projectile-load-known-projects)

  (defn projectile-project-files-custom-filter (files)
    "TODO"
    (-if-let (regex (-some->> (projectile-paths-to-ignore)
                              (--map (->> it
                                          (s-chop-prefix (file-truename (projectile-project-root)))
                                          (concat "^")))
                              (append (projectile-patterns-to-ignore))
                              (-interpose "\\|")
                              (apply #'concat)))
        (-remove (-partial #'string-match-p regex) files)
      files))

  :config
  (advice-add #'projectile-project-root :before-until
              (byte-compile (lambda (&optional _) (persp-current-project))))
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)
  (advice-add #'projectile-project-files :filter-return
              #'projectile-project-files-custom-filter)
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

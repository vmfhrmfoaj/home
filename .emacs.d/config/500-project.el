(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :commands (projectile-load-known-projects)
  :init
  (projectile-load-known-projects)

  (defun projectile-project-files-custom-filter (files)
    "TODO"
    (let ((root (file-truename (projectile-project-root))))
      (-if-let (regex (-some->> (append (projectile-project-ignored-files)
                                        (-some->> (projectile-project-ignored-directories)
                                                  (--map (concat it "/"))))
                                (-map (-partial #'s-chop-prefix root))
                                (regexp-opt)
                                (concat "^")))
          (-remove (-partial #'string-match-p regex) files)
       files)))

  :config
  (advice-add #'projectile-project-root :before-until
              (lambda (&optional _) (persp-current-project)))
  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)
  (advice-add #'projectile-project-files :filter-return
              #'projectile-project-files-custom-filter)
  (projectile-cleanup-known-projects)
  (projectile-mode 1))

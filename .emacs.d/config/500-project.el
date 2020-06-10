(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :commands (projectile-project-root)
  :config
  (defn projectile-switch-to-previous-buffer ()
    "TODO"
    (interactive)
    (condition-case nil
        (let ((cur-proj-root (or (projectile-project-root)
                                 (concat (s-chop-suffix "/" home-dir) "/"))))
          (->> (buffer-list)
               (--filter (with-current-buffer it
                           (let ((proj-root (or (projectile-project-root)
                                                (concat (s-chop-suffix "/" home-dir) "/"))))
                             (string= cur-proj-root proj-root))))
               (switch-to-previous-buffer-in)))
      (error (switch-to-previous-buffer-in (buffer-list)))))

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

  (defn projectile-switch-latest-open-project ()
    "TODO"
    (interactive)
    (let ((cur-proj-root (or (projectile-project-root)
                             (concat (s-chop-suffix "/" home-dir) "/"))))
     (-some->> (buffer-list)
       (--remove (with-current-buffer it
                   (let ((proj-root (or (projectile-project-root)
                                        (concat (s-chop-suffix "/" home-dir) "/"))))
                     (string= cur-proj-root proj-root))))
       (switch-to-previous-buffer-in))))

  (defn projectile-action-for-custom-switch-open-project ()
    "A `projectile' action for `projectile-custom-switch-open-project'."
    (let* ((cur-proj-root (projectile-project-root))
           (buf (-some->> (buffer-list)
                  (--filter (with-current-buffer it
                              ;; NOTE
                              ;;  `projectile-switch-open-project' will overwrite `default-directory' variable.
                              (let* ((default-directory (or (-some-> (buffer-file-name) (file-name-directory)) ""))
                                     (proj-root (or (projectile-project-root)
                                                    (concat (s-chop-suffix "/" home-dir) "/"))))
                                (string= cur-proj-root proj-root))))
                  (switch-to-previous-buffer-in))))
      (unless buf
        (projectile-find-file))))

  (defn projectile-custom-switch-open-project (&optional arg)
    "TODO"
    (interactive)
    (let ((projectile-switch-project-action #'projectile-action-for-custom-switch-open-project))
      (projectile-switch-open-project)))

  (defn projectile-custom-project-name (project-root)
    "TODO"
    (if (string= home-dir (s-chop-suffix "/" project-root))
        "home"
      (file-name-nondirectory (directory-file-name project-root))))

  (defn projectile-kill-new-buffer-file-name ()
    "TODO"
    (interactive)
    (-when-let (file-name (buffer-file-name))
      (message (kill-new (-if-let (root (projectile-project-root))
                             (s-chop-prefix root file-name)
                           file-name)))))

  (setq projectile-completion-system 'helm
        projectile-enable-cachig t
        projectile-project-name-function #'projectile-custom-project-name)

  (add-hook 'find-file-hook
            (lambda ()
              (setq-local dumb-jump-project
                          (-> buffer-file-name
                              (file-name-directory)
                              (projectile-project-root)))))

  (advice-add #'projectile-project-files :filter-return
              #'projectile-project-files-custom-filter)

  (projectile-mode 1)

  ;; NOTE
  ;;  Switch the default project
  (let ((projectile-switch-project-action (lambda (&rest _))))
    (projectile-switch-project-by-name "~/")))

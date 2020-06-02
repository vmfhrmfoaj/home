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
        (switch-to-previous-buffer-in (projectile-project-buffers))
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
    (let ((cur-proj-root (projectile-project-root)))
     (-some->> (buffer-list)
       (sort-buffer-by-visit-time)
       (--filter (with-current-buffer it
                   (when-let ((proj-root (projectile-project-root)))
                     (not (string= cur-proj-root proj-root)))))
       (-first-item)
       (switch-to-buffer))))

  (defn projectile-action-for-custom-switch-open-project ()
    "A `projectile' action for `projectile-custom-switch-open-project'."
    (let* ((visible-bufs (-map #'window-buffer (window-list)))
           (buf (-some->> (projectile-project-buffers)
                  (sort-buffer-by-visit-time)
                  (--remove (-contains? visible-bufs it))
                  (-first-item))))
      (if buf
          (switch-to-buffer buf)
        (projectile-find-file))))

  (defn projectile-custom-switch-open-project (&optional arg)
    "TODO"
    (interactive)
    (let ((projectile-switch-project-action #'projectile-action-for-custom-switch-open-project))
      (projectile-switch-open-project)))

  (setq projectile-completion-system 'helm
        projectile-enable-cachig t)

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

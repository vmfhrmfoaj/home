(use-package persp-mode
  :ensure t
  :init
  (defvar persp-org-name "@Org"
    "TODO")

  (defn persp-current-name ()
    "TODO"
    (safe-persp-name (get-frame-persp)))

  (defn persp-current-project ()
    "TODO"
    (let ((persp-name (persp-current-name)))
      (cond
       ((string-equal persp-org-name persp-name)
        (or (-some->> (bound-and-true-p org-directory)
                      (s-chop-suffix "/")
                      (s-append "/"))
            persp-nil-name))
       ((file-exists-p persp-name)
        persp-name)
       (t nil))))

  (defn helm-persp-do-create-&-switch-project (proj)
    "TODO"
    (interactive)
    (let ((persp-reset-windows-on-nil-window-conf t))
      (persp-switch proj)
      (projectile-switch-project-by-name proj)))

  (defn helm-persp-create-&-switch-project ()
    "TODO"
    (interactive)
    (helm :sources
          (helm-build-in-buffer-source "*Helm Create/Switch Project Perspective*"
            :data projectile-known-projects
            :fuzzy-match helm-projectile-fuzzy-match
            :action '(("Create/Switch to Project Perspective" . helm-persp-do-create-&-switch-project)))))

  (defn helm-persp ()
    "TODO"
    (interactive)
    (helm :preselect (persp-current-name)
          :sources
          (helm-build-in-buffer-source (format "*Helm Perspective*: %s" (persp-current-name))
            :data (persp-names)
            :fuzzy-match t
            :action '(("Switch to Perspective" . persp-switch)))))

  (defn persp-switch-to-default ()
    "TODO"
    (interactive)
    (persp-switch persp-nil-name))

  (defn persp-kill-cur-persp ()
    "TODO"
    (interactive)
    (persp-kill (persp-current-name)))

  (defn persp-add-buffer-without-switch (&optional buf)
    "TODO"
    (persp-add-buffer (or buf (current-buffer))
                      (get-current-persp)
                      nil))

  (defn persp-add-all-proj-buffer (&rest _)
    "TODO"
    (-when-let (cur-root (get-current-persp))
      (let* ((root (aref cur-root 1))
             (root_ (if (string-prefix-p "/" root)
                        (concat "~/" (file-relative-name root (getenv "HOME")))
                      (file-truename root))))
        (dolist (buf (--filter (or (projectile-project-buffer-p it root_)
                                   (projectile-project-buffer-p it root))
                               (buffer-list)))
          (persp-add-buffer-without-switch buf)))))

  (defvar persp-sorted-names nil
    "TODO")

  (defn persp-update-sorted-names (&rest _)
    "TODO"
    (let ((cur (persp-current-name)))
      (-update->> persp-sorted-names
                  (remove cur)
                  (cons cur))))

  (defn persp-switch-to-last-selected-persp ()
    "TODO"
    (interactive)
    (let ((names persp-sorted-names)
          (last-persp nil))
      (while (and names (not last-persp))
        (let ((it (car names)))
          (if (and (member it (persp-names))
                   (not (string-equal it (persp-current-name))))
              (setq last-persp it)
            (setq names (cdr names)))))
      (when last-persp
        (persp-switch last-persp)
        (when global-hl-line-mode
          (hl-line-mode 1)))))

  (defn persp-add-new-for-org ()
    "TODO"
    (let ((layout (persp-add-new persp-org-name)))
      (persp-switch persp-org-name)
      (dolist (buf (append (->> org-agenda-files (-map #'find-file-noselect))
                           (->> (buffer-list) (--filter (with-current-buffer it
                                                          (derived-mode-p 'org-agenda-mode))))))
        (persp-add-buffer buf))
      (org-agenda-list)))

  (defn persp-switch-to-org ()
    "TODO"
    (interactive)
    (if (persp-get-by-name persp-org-name *persp-hash* nil)
        (persp-switch persp-org-name)
      (persp-add-new-for-org)))

  :config
  (setq persp-autokill-buffer-on-remove #'kill-weak
        persp-auto-resume-time -1
        ;; TODO
        ;;  change mode-line format for `persp-mode'
        ;; persp-lighter "Ⓟ"
        persp-before-switch-functions #'persp-update-sorted-names
        persp-lighter nil
        persp-nil-name "~/"
        persp-last-selected-persp-name persp-nil-name
        persp-set-ido-hooks t
        wg-morph-on nil)

  (advice-add #'persp-switch :after #'persp-add-all-proj-buffer)
  (add-hook 'magit-diff-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'magit-log-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'magit-status-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'after-init-hook (-partial #'persp-mode 1)))

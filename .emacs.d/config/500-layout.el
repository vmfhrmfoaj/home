(use-package persp-mode
  :ensure t
  :init
  (defun persp-current-name ()
    (safe-persp-name (get-frame-persp)))

  (defun persp-current-project ()
    (let ((persp-name (persp-current-name)))
      (when (and (file-exists-p persp-name)
                 (not (string-equal persp-name persp-nil-name)))
        persp-name)))

  (defun helm-persp-create-&-switch-project ()
    "TODO"
    (interactive)
    (helm :sources
          (helm-build-in-buffer-source "*Helm Create/Switch Project Perspective*"
            :data projectile-known-projects
            :fuzzy-match helm-projectile-fuzzy-match
            :action '(("Create/Switch to Project Perspective" .
                       (lambda (proj)
                         (let ((persp-reset-windows-on-nil-window-conf t))
                           (persp-switch proj)
                           (projectile-switch-project-by-name proj))))))))

  (defun helm-persp ()
    "TODO"
    (interactive)
    (helm :sources
          (helm-build-in-buffer-source (format "*Helm Perspective: %s*" (persp-current-name))
            :data (persp-names)
            :fuzzy-match t
            :action '(("Switch to Perspective" . persp-switch)))))

  (defun persp-switch-to-default ()
    "TODO"
    (interactive)
    (persp-switch persp-nil-name))

  (defun persp-kill-cur-persp ()
    "TODO"
    (interactive)
    (persp-kill (persp-current-name)))

  (defun persp-add-buffer-without-switch (&optional buf)
    "TODO"
    (persp-add-buffer (or buf (current-buffer))
                      (get-current-persp)
                      nil))

  (defun persp-add-all-proj-buffer (&rest _)
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

  (defun persp-update-sorted-names (&rest _)
    "TODO"
    (let ((cur (persp-current-name)))
      (-update->> persp-sorted-names
                  (remove cur)
                  (cons cur))))

  (defun persp-switch-to-last-selected-persp ()
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
        (persp-switch last-persp))))

  :config
  (setq persp-autokill-buffer-on-remove #'kill-weak
        persp-auto-resume-time -1
        ;; TODO
        ;;  change mode-line format for `persp-mode'
        ;; persp-lighter "Ⓟ"
        persp-before-switch-functions #'persp-update-sorted-names
        persp-lighter '(:eval (format "persp(%s)"
                                      (save-match-data
                                        (let ((cur-name (persp-current-name)))
                                          (if (string-match "/\\([^/]+\\)/?$" cur-name)
                                              (match-string 1 cur-name)
                                            cur-name)))))
        persp-nil-name "~/"
        persp-last-selected-persp-name persp-nil-name
        persp-set-ido-hooks t
        wg-morph-on nil)

  (advice-add #'persp-switch :after #'persp-add-all-proj-buffer)
  (add-hook 'magit-diff-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'magit-log-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'magit-status-mode-hook #'persp-add-buffer-without-switch)
  (add-hook 'after-init-hook (-partial #'persp-mode 1)))

(use-package persp-mode
  :ensure t
  :init
  (defun persp-current-name ()
    (safe-persp-name (get-frame-persp)))

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

  (defun persp-safe-add-buffer (&optional buf)
    "TODO"
    (save-window-excursion
      (save-match-data
        (save-mark-and-excursion
          (persp-add-buffer (or buf (current-buffer)))))))

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
          (persp-safe-add-buffer buf)))))

  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-resume-time -1
        ;; TODO
        ;;  change mode-line format for `persp-mode'
        ;; persp-lighter "Ⓟ"
        persp-lighter '(:eval (format "{%s}"
                                      (->> (persp-current-name)
                                           (s-chop-suffix "/")
                                           (file-name-base))))
        persp-nil-name "Default"
        persp-set-ido-hooks t
        wg-morph-on nil)

  (advice-add #'persp-switch :after #'persp-add-all-proj-buffer)
  (add-hook 'magit-diff-mode-hook #'persp-safe-add-buffer)
  (add-hook 'magit-log-mode-hook #'persp-safe-add-buffer)
  (add-hook 'magit-status-mode-hook #'persp-safe-add-buffer)
  (add-hook 'after-init-hook
            (lambda ()
              (persp-mode 1))))

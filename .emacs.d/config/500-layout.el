(use-package persp-mode
  :ensure t
  :init
  (defun helm-persp-create-&-switch-project ()
    "TODO"
    (interactive)
    (helm :sources
          (helm-build-in-buffer-source "*Helm Create/Switch Project Perspective*"
            :data projectile-known-projects
            :fuzzy-match #'helm-fuzzy-match
            :action '(("Create/Switch to Project Perspective" .
                       (lambda (proj)
                         (let ((persp-reset-windows-on-nil-window-conf t))
                           (persp-switch proj)
                           (projectile-switch-project-by-name proj))))))))

  (defun helm-persp ()
    "TODO"
    (interactive)
    (helm :sources
          (helm-build-in-buffer-source "*Helm Perspective*"
            :data (persp-names)
            :fuzzy-match t
            :action '(("Switch to Perspective" . persp-switch)))))

  (defun persp-switch-to-default ()
    "TODO"
    (interactive)
    (persp-switch persp-nil-name))

  (defun persp-current-name ()
    (safe-persp-name (get-frame-persp)))

  (defun persp-kill-cur-persp ()
    "TODO"
    (interactive)
    (persp-kill (persp-current-name)))

  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-resume-time -1
        persp-nil-name "Default"
        persp-set-ido-hooks t
        wg-morph-on nil)

  (add-hook 'after-init-hook
            (lambda ()
              (persp-mode 1))))

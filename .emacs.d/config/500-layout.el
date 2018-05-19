(use-package persp-mode
  :ensure t
  :init
  (defvar helm-persp-switch-source
    (helm-build-in-buffer-source "*Helm Create/Switch Project Perspective*"
      :candidates (lambda () projectile-known-projects)
      :fuzzy-match helm-projectile-fuzzy-match
      :action '(("Create/Switch to Project Perspective" .
                 (lambda (proj)
                   (let ((persp-reset-windows-on-nil-window-conf t))
                     (persp-switch proj)
                     (projectile-switch-project-by-name proj))))))
    "TODO")

  (defun helm-persp-create-&-switch-project ()
    "TODO"
    (interactive)
    (helm :sources helm-persp-switch-source))

  (defvar helm-persp-source
    (helm-build-in-buffer-source "*Helm Perspective*"
      :candidates (lambda () (persp-names))
      :fuzzy-match t
      :action '(("Switch to Perspective" . persp-switch)))
    "TODO")

  (defun helm-persp ()
    "TODO"
    (interactive)
    (helm :sources helm-persp-source))

  (defun persp-switch-to-default ()
    "TODO"
    (interactive)
    (persp-switch persp-nil-name))

  (defun persp-kill-cur-persp ()
    "TODO"
    (interactive)
    (persp-kill (safe-persp-name (get-frame-persp))))

  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-resume-time -1
        persp-nil-name "Default"
        persp-set-ido-hooks t
        wg-morph-on nil)

  (add-hook 'after-init-hook
            (lambda ()
              (persp-mode 1))))

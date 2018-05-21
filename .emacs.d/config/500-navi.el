(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-find-dir
             helm-projectile-find-file
             helm-projectile-switch-project))

(use-package neotree
  :ensure t
  :init
  (defun neotree-project-dir ()
    (interactive)
    (let ((proj-dir projectile-project-root)
          (buf-dir (file-name-directory (or buffer-file-name "~/"))))
      (neotree-dir (or proj-dir buf-dir))))

  :config
  (setq neo-theme 'icons))

(use-package dumb-jump
  :ensure t)

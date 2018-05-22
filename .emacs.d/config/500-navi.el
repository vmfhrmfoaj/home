(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-find-dir
             helm-projectile-find-file
             helm-projectile-switch-project))

(use-package neotree
  :ensure t
  :init
  (defun neo-buffer--back (path _)
    "TODO"
    (when (neo-buffer--expanded-node-p path)
      (neo-buffer--toggle-expand path)
      (neotree-refresh)))

  (defun neotree-back ()
    "TODO"
    (interactive)
    (neotree-select-up-node)
    (neo-buffer--execute nil null-fn #'neo-buffer--back))

  (defun neotree-project-dir ()
    "TODO"
    (interactive)
    (let ((proj-dir (projectile-project-root))
          (buf-dir (file-name-directory (or buffer-file-name "~/"))))
      (neotree-dir (or proj-dir buf-dir))))

  :config
  (setq neo-auto-indent-point t
        neo-keymap-style 'concise
        neo-theme 'icons))

(use-package dumb-jump
  :ensure t)

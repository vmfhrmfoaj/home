(use-package helm-swoop
  :ensure t
  :defer t
  :commands (helm-swoop))

(use-package helm-ag
  :ensure t
  :defer t
  :commands (helm-do-ag
             helm-do-ag-project-root)
  :config
  (setq helm-ag-base-command "rg --no-heading"
	      helm-ag-use-emacs-lisp-regexp t))

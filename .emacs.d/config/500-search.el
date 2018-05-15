(use-package helm-swoop
  :ensure t)

(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-base-command "rg --no-heading"
	helm-ag-use-emacs-lisp-regexp t))

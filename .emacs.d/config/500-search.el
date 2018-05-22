(use-package helm-swoop
  :ensure t
  :defer t
  :commands (helm-swoop)
  :config
  (setq helm-swoop-split-window-function #'helm-default-display-buffer
        helm-swoop-pre-input-function (-const "")))

(use-package helm-ag
  :ensure t
  :defer t
  :commands (helm-do-ag
             helm-do-ag-project-root)

  :config
  (setq helm-ag-base-command "rg --no-heading"
	      helm-ag-use-emacs-lisp-regexp t))

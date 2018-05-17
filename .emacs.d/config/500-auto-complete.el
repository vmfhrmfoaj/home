(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (global-company-mode 1))

(use-package helm-company
  :ensure t
  :defer t
  :commands (helm-company)
  :config
  (define-key company-active-map (kbd "C-s") #'helm-company))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (global-company-mode 1))

(use-package helm-company
  :ensure t
  :after (company helm))

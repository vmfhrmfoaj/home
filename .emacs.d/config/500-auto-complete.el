(use-package company
  :ensure t
  :init
  (defun company-complete-selection-and-insert-space ()
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  :config
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (setq company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (global-company-mode 1))

(use-package helm-company
  :ensure t
  :after (company helm))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package evil-multiedit
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :init
  (defun sp-wrap-sexp (&optional arg)
    "TODO"
    (interactive "P")
    (sp-wrap-with-pair "("))

  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package whitespace
  :config
  (setq-default show-trailing-whitespace t
                whitespace-line-column 120))

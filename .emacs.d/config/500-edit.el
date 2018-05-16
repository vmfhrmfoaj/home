(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package smartparens-config
  :ensure smartparens
  :config
  (defun sp-wrap-sexp (&optional arg)
    (interactive "P")
    (sp-wrap-with-pair "("))

  (byte-compile #'sp-wrap-sexp)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package whitespace
  :config
  (setq-default show-trailing-whitespace t))

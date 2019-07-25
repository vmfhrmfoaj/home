(use-package sh-script
  :defer t
  :mode ("\\.env\\'" . sh-mode)
  :config
  (setq sh-basic-offset 2
        sh-indentation 2
        smie-indent-basic 2)
  (add-hook 'sh-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

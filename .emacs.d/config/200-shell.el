(use-package sh-script
  :defer t
  :mode ("\\.env\\'" . sh-mode)
  :config
  (setq sh-basic-offset 4
        sh-indentation 4
        smie-indent-basic 4)
  (add-hook 'sh-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

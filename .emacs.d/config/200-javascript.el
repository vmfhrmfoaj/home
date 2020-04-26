(use-package js
  :defer t
  :config
  (setq-default js-indent-level 4)
  (add-hook 'js-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package js2-mode
  :disabled t
  :ensure t
  :defer t
  :mode (("\\.jsm?\\'" . js2-mode)
         ("\\.json\\'" . js2-mode)
         ("\\.jsx\\'"  . js2-jsx-mode))

  :config
  (defconst js--prettify-symbols-alist nil)
  (setq-default js-indent-level 4
                js2-basic-offset 2)
  (add-hook 'js2-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

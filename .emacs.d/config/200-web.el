;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package css-mode
  :defer t
  :mode "\\.wxss\\'"
  :config
  (setq css-indent-offset 2))

(use-package js
  :defer t
  :config
  (setq-default js-indent-level 4)
  (add-hook 'js-mode-hook
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package vue-mode
  :disabled t
  :ensure t
  :defer t)

(use-package web-beautify
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.\\(html\\|eex\\|wxml\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (with-eval-after-load "smartparens"
    (add-hook 'web-mode-hook
              (lambda ()
                (-update->> web-mode-auto-pairs
                            (--map (let ((a (car it))
                                        (b (substring (cdr it) 0 -1)))
                                    `(,a . ,b))))))))

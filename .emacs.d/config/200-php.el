;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package php-mode
  :ensure t
  :defer t
  :mode ("\\.php\\|ant\\'" . php-mode)
  :init
  (with-eval-after-load "dumb-jump"
    (add-to-list 'dumb-jump-language-file-exts
                 '(:language "php" :ext "ant" :agtype "php" :rgtype "php")))

  (add-hook 'php-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point))
              ;; (make-local-variable 'font-lock-extend-region-functions)
              ;; (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
              )))

(use-package psysh
  :disabled t
  :ensure t
  :defer t
  :config
  (defun psysh-show ()
    (interactive)
    (-when-let (buf (get-buffer (concat "*" (car (psysh--detect-buffer)) "*")))
      (switch-to-buffer buf)))

  (advice-add #'psysh-restart :before #'psysh-show))

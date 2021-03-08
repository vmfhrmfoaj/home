;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package sh-script
  :defer t
  :mode ("\\.env\\'" . sh-mode)
  :config
  (setq sh-basic-offset 4
        sh-indentation 4
        smie-indent-basic 4))

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local completion-ignore-case t)))

  (add-hook 'eshell-post-command-hook
            (lambda ()
              (let ((evil-move-cursor-back nil))
                (evil-normal-state)))))

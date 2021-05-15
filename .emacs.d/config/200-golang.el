;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package go-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'go-mode nil t))

  :config
  (setq go-fontify-function-calls nil))

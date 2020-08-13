;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package "dockerfile-mode"
  :ensure t
  :defer t
  :mode "\\.Dockerfile\\'")

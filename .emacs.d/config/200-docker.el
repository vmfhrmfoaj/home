;; -*- lexical-binding: t; -*-

(eval-and-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package "dockerfile-mode"
  :ensure t
  :defer t
  :mode "\\.Dockerfile\\'")

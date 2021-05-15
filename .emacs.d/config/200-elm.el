;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package elm-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'elm-mode nil t)))

;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package kotlin-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'kotlin-mode nil t)))

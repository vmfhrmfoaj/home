;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package kotlin-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'kotlin-mode nil t)))

;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package quelpa
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'quelpa nil t)))

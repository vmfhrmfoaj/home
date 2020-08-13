;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package cmake-ide
  :ensure t
  :hook (cmake-mode . cmake-ide-setup))

(use-package cmake-mode
  :ensure t
  :defer t)

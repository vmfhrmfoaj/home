;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\.Dockerfile\\'")

(use-package docker-tramp
  :ensure t
  :defer t)

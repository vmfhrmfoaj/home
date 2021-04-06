;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package cmake-ide
  :ensure t
  :hook (cmake-mode . cmake-ide-setup)
  :init
  (eval-when-compile (require 'cmake-ide nil t)))

(use-package cmake-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'cmake-mode nil t)))

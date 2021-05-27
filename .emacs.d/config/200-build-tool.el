;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'cmake-ide nil t)
  (require 'cmake-mode nil t))

(use-package cmake-ide
  :ensure t
  :hook (cmake-mode . cmake-ide-setup))

(use-package cmake-mode
  :ensure t
  :defer t)

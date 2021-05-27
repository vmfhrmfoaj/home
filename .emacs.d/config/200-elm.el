;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'elm-mode nil t))

(use-package elm-mode
  :disabled t
  :ensure t
  :defer t)

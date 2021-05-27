;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'go-mode nil t))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq go-fontify-function-calls nil))

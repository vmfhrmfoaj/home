;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'async nil t)
  (require 'queue nil t)
  (require 'spinner nil t)
  (require 'undo-tree nil t))

(use-package async
  :ensure t
  :config
  (dired-async-mode 1))

(use-package queue
  :ensure t
  :defer t)

(use-package spinner
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'spinner)
    (quelpa '(spinner :fetcher github :repo "Malabarba/spinner.el"))))

(use-package undo-tree
  :ensure t
  :defer t)


;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package async
  :ensure t
  :init
  (eval-when-compile (require 'async nil t))

  :config
  (dired-async-mode 1))

(use-package queue
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'queue nil t)))


(use-package spinner
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'spinner)
    (quelpa '(spinner :fetcher github :repo "Malabarba/spinner.el")))
  (eval-when-compile (require 'spinner nil t)))

(use-package undo-tree
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'undo-tree nil t)))


;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package async
  :ensure t
  :defer t
  :init
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


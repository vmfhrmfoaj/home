;;; Copyright (c) 2014-2017, Jinseop Kim

(setq custom-file "~/.emacs.d/.custom.el"
      gc-cons-threshold (* 1024 1024 128)
      inhibit-startup-screen t)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 1024 1024 32))))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-archives
      '(("org"          . "https://orgmode.org/elpa/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("org"          . 15)
        ("melpa"        . 10)
        ("melpa-stable" .  5)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package init-loader
  :ensure t
  :config
  (let ((file-name-handler-alist nil))
    (init-loader-load "~/.emacs.d/config")))

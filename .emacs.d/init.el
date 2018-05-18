;;; Copyright (c) 2014-2017, Jinseop Kim

(setq custom-file "~/.emacs.d/.custom.el"
      inhibit-startup-screen t)
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package nil t)
(with-eval-after-load "package"
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (when (require 'use-package)
    (use-package init-loader
      :ensure t
      :config
      (let ((gc-cons-threshold (* 1024 1024 512)))
        (init-loader-load "~/.emacs.d/config")))))

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'bison-mode nil t)
  (require 'capnp-mode nil t))

(use-package bison-mode
  :ensure t
  :defer t)

(use-package capnp-mode
  :disabled t
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'capnp-mode)
    (quelpa '(capnp-mode :fetcher url
                         :url "https://raw.githubusercontent.com/vmfhrmfoaj/capnproto/master/highlighting/emacs/capnp-mode.el"
                         :version original))))

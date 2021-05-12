;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package bison-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'bison-mode nil t)))

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
                         :version original)))
  (eval-when-compile (require 'capnp-mode nil t)))

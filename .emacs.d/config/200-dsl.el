;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package capnp-mode
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'capnp-mode)
    (quelpa '(capnp-mode :fetcher url
                         :url "https://raw.githubusercontent.com/vmfhrmfoaj/capnproto/master/highlighting/emacs/capnp-mode.el"
                         :version original))))

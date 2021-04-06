;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package docker
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'docker nil t)))

(use-package docker-tramp
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'docker-tramp nil t)))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\.Dockerfile\\'"
  :init
  (eval-when-compile (require 'dockerfile-mode nil t)))

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package docker
  :ensure t
  :defer t)

(use-package docker-tramp
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\.Dockerfile\\'")

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package cc-mode
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  (setq-default c-backslash-column 80
                c-backslash-max-column 120)

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'case-label '+)
              (unless (and (fboundp #'editorconfig-core-get-nearest-editorconfig)
                           (editorconfig-core-get-nearest-editorconfig default-directory))
                (setq c-basic-offset 4))
              (when (member major-mode '(c-mode c++-mode))
                (c-toggle-auto-newline -1)))
            :append))

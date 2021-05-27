;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'cargo nil t)
  (require 'rust-mode nil t))

(use-package cargo
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (defvar cargo-home (or (getenv "CARGO_HOME")
                         (concat home-dir "/.cargo")))

  (setq rust-format-on-save t
        rust-format-show-buffer nil)

  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t)))

  (with-eval-after-load "smartparens"
    (sp-local-pair '(rust-mode) "'" "'" :actions nil)))

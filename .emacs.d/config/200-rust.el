;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package cargo
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'cargo nil t)))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'rust-mode nil t))

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

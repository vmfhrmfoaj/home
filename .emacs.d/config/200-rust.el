;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package cargo
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (defvar cargo-home (or (getenv "CARGO_HOME")
                         (concat home-dir "/.cargo")))

  (setq rust-format-on-save nil
        rust-format-show-buffer nil)

  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t)))

  (sp-local-pair '(rust-mode) "'" "'" :actions nil))

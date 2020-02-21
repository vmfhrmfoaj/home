(use-package cargo
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (defvar cargo-home (or (getenv "CARGO_HOME")
                         (concat home-dir "/.cargo")))

  (add-hook 'rust-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point))))

  (sp-local-pair '(rust-mode) "'" "'" :actions nil))

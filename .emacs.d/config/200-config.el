;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package rpm-spec-mode
  :ensure t
  :defer t
  :mode "\\.rpm\\'"
  :config
  (when (require 'highlight-numbers nil t)
    (puthash 'rpm-spec-mode "\\<[0-9]+\\>" highlight-numbers-modelist))

  (add-hook 'rpm-spec-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t))))

(use-package toml-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode)) ; for systemd service file

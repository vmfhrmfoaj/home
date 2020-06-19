(use-package rpm-spec-mode
  :ensure t
  :defer t
  :mode "\\.rpm\\'"
  :config
  (when (require 'highlight-numbers nil t)
    (puthash 'rpm-spec-mode "\\<[0-9]+\\>" highlight-numbers-modelist)))

(use-package toml-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode)) ; for systemd service file

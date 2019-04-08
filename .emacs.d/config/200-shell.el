(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2
        sh-indentation 2
        smie-indent-basic 2))

(add-to-list 'auto-mode-alist '("/\\.env\\'" . sh-mode)) ; for systemd service file

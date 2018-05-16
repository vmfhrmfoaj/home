(setq-default indent-tabs-mode nil
              tab-width 2)

(setq ring-bell-function 'ignore
      initial-major-mode 'org-mode
      initial-scratch-message "")

(prefer-coding-system 'utf-8)
(toggle-truncate-lines 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

(when window-system
  (include-shell-var-in "~/.profile"))

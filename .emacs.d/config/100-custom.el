(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\|NeoTree\\)"
      initial-major-mode 'org-mode
      initial-scratch-message ""
      ring-bell-function 'ignore)

(advice-add #'select-frame      :after #'update-buf-visit-time)
(advice-add #'select-window     :after #'update-buf-visit-time)
(advice-add #'set-window-buffer :after #'update-buf-visit-time)
(advice-add #'switch-to-buffer  :after #'update-buf-visit-time)

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

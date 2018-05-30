(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

(setq backup-directory-alist `(("" . ,(concat (getenv "HOME") "/.emacs.d/saves/")))
      exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\|NeoTree\\|Org todo\\)"
      initial-major-mode 'org-mode
      initial-scratch-message ""
      ring-bell-function 'ignore)

(prefer-coding-system 'utf-8)
(electric-indent-mode -1)
(when (eq 'gnu/linux system-type)
  (menu-bar-mode -1))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

(when window-system
  (include-shell-var-in "~/.profile"))

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 64)
                  gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect))
            (advice-add #'select-frame      :after #'update-buf-visit-time)
            (advice-add #'select-window     :after #'update-buf-visit-time)
            (advice-add #'set-window-buffer :after #'update-buf-visit-time)
            (advice-add #'switch-to-buffer  :after #'update-buf-visit-time)))

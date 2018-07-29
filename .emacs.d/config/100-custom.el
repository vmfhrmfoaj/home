(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

(let ((backup-dir (concat (getenv "HOME") "/.emacs.d/saves/")))
  (setq auto-save-file-name-transforms `((".*" ,backup-dir t))
        backup-directory-alist `((".*" . ,backup-dir))
        create-lockfiles nil
        exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\|NeoTree\\|Org todo\\)"
        initial-major-mode 'text-mode
        initial-scratch-message ""
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)

(global-subword-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

(add-hook 'after-init-hook
          (lambda ()
            (advice-add #'select-frame      :after #'update-buf-visit-time)
            (advice-add #'select-window     :after #'update-buf-visit-time)
            (advice-add #'set-window-buffer :after #'update-buf-visit-time)
            (advice-add #'switch-to-buffer  :after #'update-buf-visit-time)
            (setq gc-cons-threshold (* 1024 1024 128)
                  gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect))
            (fringe-mode 15))
          :append)

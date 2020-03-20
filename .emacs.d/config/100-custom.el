(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(let ((backup-dir (concat home-dir "/.emacs.d/saves/")))
  (setq auto-revert-interval 0.5
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        backup-directory-alist `((".*" . ,backup-dir))
        blink-cursor-blinks 0
        blink-cursor-delay 3
        create-lockfiles nil
        exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\|NeoTree\\)"
        initial-major-mode 'text-mode
        initial-scratch-message ""
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)

(blink-cursor-mode 1)
(global-subword-mode 1)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

(setq resize-mini-windows t)

(add-hook 'focus-in-hook #'blink-cursor-start)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'after-init-hook
          (lambda ()
            (advice-add #'select-frame      :after #'update-buf-visit-time)
            (advice-add #'select-window     :after #'update-buf-visit-time)
            (advice-add #'set-window-buffer :after #'update-buf-visit-time)
            (advice-add #'switch-to-buffer  :after #'update-buf-visit-time)
            (setq gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect)))
          :append)

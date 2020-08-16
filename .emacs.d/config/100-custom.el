;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

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
        comment-fill-column 100
        create-lockfiles nil
        exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\)"
        initial-major-mode 'text-mode
        initial-scratch-message ""
        read-process-output-max (* 1024 1024)
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)

(define-category ?U "Uppercase")
(define-category ?u "Lowercase")
(modify-category-entry (cons ?A ?Z) ?U)
(modify-category-entry (cons ?a ?z) ?u)
(push '(?u . ?U) evil-cjk-word-separating-categories)
(global-subword-mode 1)

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
            (advice-add #'switch-to-buffer :around
                        (lambda (fn buf &rest args)
                          (if-let ((win (->> (window-list)
                                             (--filter (-> it (window-buffer) (eq buf)))
                                             (-first-item))))
                              (let ((cur-buf (current-buffer)))
                                (with-selected-window win
                                  (apply fn cur-buf args)))
                            (apply fn buf args))
                          (update-buf-visit-time)))
            (advice-add #'pop-to-buffer :around
                        (lambda (fn buf &rest args)
                          (if-let ((win (->> (window-list)
                                             (--filter (-> it (window-buffer) (eq buf)))
                                             (-first-item))))
                              (select-window win)
                            (apply fn buf args))))
            (setq gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect)))
          :append)

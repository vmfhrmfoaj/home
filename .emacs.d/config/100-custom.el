;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(let ((backup-dir (concat home-dir "/.emacs.d/saves/")))
  (setq auto-revert-interval 0.5
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        backup-directory-alist `((".*" . ,backup-dir))
        blink-cursor-blinks 0
        blink-cursor-delay 0
        comment-fill-column 100
        create-lockfiles nil
        default-input-method "korean-hangul"
        exclude-prev-buf-regex "^\\(\\s-*\\*\\|\\s-*markdown-code-fontification:\\|\\s-*magit\\(?:-[a-z]+\\)?:\\)"
        initial-major-mode 'text-mode
        initial-scratch-message ""
        read-process-output-max (* 1024 1024)
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when-let ((input-method (--first (string= "korean-hangul" (car it)) input-method-alist)))
  (setf (nth 3 input-method) "Korean"))

(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)
(mouse-avoidance-mode 'banish)

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
            (add-hook 'buffer-list-update-hook #'update-buf-visit-time)
            (advice-add #'switch-to-buffer :around
                        (lambda (fn buf &rest args)
                          (if-let ((win (->> (window-list)
                                             (--filter (-> it (window-buffer) (eq buf)))
                                             (-first-item))))
                              (let ((cur-buf (current-buffer)))
                                (with-selected-window win
                                  (apply fn cur-buf args)))
                            (apply fn buf args))))
            (advice-add #'pop-to-buffer :around
                        (lambda (fn buf &rest args)
                          (if-let ((win (->> (window-list)
                                             (--filter (-> it (window-buffer) (eq buf)))
                                             (-first-item))))
                              (select-window win)
                            (apply fn buf args))))
            (setq gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect)))
          :append)

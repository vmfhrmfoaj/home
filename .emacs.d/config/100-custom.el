(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq backup-directory-alist `(("" . ,(concat (getenv "HOME") "/.emacs.d/saves/")))
      exclude-alt-buf-regex "^\\s-*\\*\\s-*\\([Hh]elm\\|which-key\\|NeoTree\\|Org todo\\)"
      initial-major-mode 'org-mode
      initial-scratch-message ""
      ring-bell-function 'ignore)

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

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 64))
            (advice-add #'select-frame      :after #'update-buf-visit-time)
            (advice-add #'select-window     :after #'update-buf-visit-time)
            (advice-add #'set-window-buffer :after #'update-buf-visit-time)
            (advice-add #'switch-to-buffer  :after #'update-buf-visit-time)))

;; for improving the performance
(setq font-lock-idle-time 0.1
      font-lock-idle-timer nil
      font-lock-idle-avoid-buf-regex (regexp-opt '("org-src-fontification"))
      font-lock-idle-avoid-cmds '(comment-dwim undo undo-tree-undo)
      font-lock-idle-start nil
      font-lock-idle-end nil
      gc-idle-timer (run-with-idle-timer 120 t #'garbage-collect))
(make-local-variable 'font-lock-idle-start)
(make-local-variable 'font-lock-idle-end)
(make-local-variable 'font-lock-idle-timer)
(advice-add #'jit-lock-after-change :around
            (byte-compile
             (lambda (fn start end old-len)
               (if (or (not font-lock-idle-time)
                       (string-match-p font-lock-idle-avoid-buf-regex (buffer-name))
                       (-contains? font-lock-idle-avoid-cmds this-command))
                   (funcall fn start end old-len)
                 (when (timerp font-lock-idle-timer)
                   (let ((timer-fn (prog1 (timer--function font-lock-idle-timer)
                                     (cancel-timer font-lock-idle-timer)))
                         (start_ (or font-lock-idle-start start))
                         (end_   (or font-lock-idle-end   end)))
                     (when (= end_ start)  (setq start start_))
                     (when (= end  start_) (setq end end_))
                     (when (or (< end_   start)
                               (< end    start_)
                               (< start_ start  end_ end)
                               (< start  start_ end  end_))
                       (funcall timer-fn))))
                 (setq font-lock-idle-start start
                       font-lock-idle-end   end
                       font-lock-idle-timer
                       (run-with-idle-timer font-lock-idle-time nil
                                            (lexical-let ((args (list start end old-len))
                                                          (buf (current-buffer))
                                                          (fn fn))
                                              (lambda ()
                                                (setq font-lock-idle-timer nil)
                                                (ignore-errors
                                                  (with-current-buffer buf
                                                    (apply fn args)))))))))))

(add-hook 'after-save-hook #'rsync-remote-dir)

(use-package atomic-chrome
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'atomic-chrome-start-server))

(use-package linum-relative
  :ensure t
  :init
  (defun set-linum-rel-fmt-for-cur-file ()
    "TODO"
    (setq-local linum-relative-format
                (concat "%"
                        (-> (count-lines (point-min) (point-max))
                            (number-to-string)
                            (length)
                            (min 5)
                            (max 3)
                            (number-to-string))
                        "s")))

  (defun linum-delay-schedule ()
    (unless (eq 'self-insert-command this-command)
      (when linum-schedule-timer
        (cancel-timer linum-schedule-timer))
      (let ((timer (run-with-idle-timer
                    linum-delay nil
                    (lambda ()
                      (setq linum-schedule-timer nil)
                      (linum-update-current)))))
        (setq-local linum-schedule-timer timer))))

  :config
  (setq linum-delay 0.05
        linum-relative-current-symbol ""
        linum-schedule-timer nil)
  (advice-add #'linum-schedule :override #'linum-delay-schedule)
  (add-hook 'find-file-hook #'set-linum-rel-fmt-for-cur-file)
  (add-hook 'prog-mode-hook #'linum-relative-on))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package server
  :defer t
  :init
  (add-hook 'after-init-hook #'server-start))

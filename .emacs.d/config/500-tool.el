(add-hook 'after-save-hook #'rsync-remote-dir)
(add-hook 'after-revert-hook #'rsync-remote-dir)
(add-hook 'emacs-startup-hook
          (lambda ()
            "report the startup time."
            (message "Emacs ready in %s(gc: %.2f seconds)."
                     (emacs-init-time)
                     (/ gc-elapsed gcs-done)))
          :append)

(use-package atomic-chrome
  :ensure t
  :defer t
  :init
  (defun atomic-chrome-setup ()
    (remove-hook 'focus-out-hook #'atomic-chrome-setup)
    (require 'atomic-chrome))

  (add-hook 'focus-out-hook #'atomic-chrome-setup)

  :config
  (atomic-chrome-start-server))

(use-package display-line-numbers
  :disabled t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers--turn-on)

  :config
  (setq display-line-numbers-type 'relative
        display-line-numbers-width 3
        display-line-numbers-width-start t))

(use-package ediff
  :defer t
  :init
  (defvar ediff--exclude-mode-status nil
    "TODO")

  (defvar ediff--win-conf nil
    "TODO")

  (defun ediff-addtional-setup (&rest _)
    "TODO"
    (setq ediff--exclude-mode-status (-map #'symbol-value ediff-exclude-modes)
          ediff--win-conf (current-window-configuration))
    (disable-modes ediff-exclude-modes)
    (toggle-frame-fullscreen))

  (defun ediff-addtional-cleanup (&rest _)
    "TODO"
    (restore-modes ediff-exclude-modes ediff--exclude-mode-status)
    (-when-let (conf ediff--win-conf)
      (setq ediff--win-conf nil)
      (set-window-configuration conf))
    (toggle-frame-fullscreen))

  :config
  ;; NOTE
  ;;  prevent to calculate the width of the window in `ediff-setup-windows-plain-compare' function.
  (setq ediff-exclude-modes '(zoom-mode)
        ediff-split-window-function #'split-window-right)
  (advice-add #'ediff-setup :before #'ediff-addtional-setup)
  (advice-add #'ediff-quit  :after  #'ediff-addtional-cleanup))

(use-package expand-region
  :ensure t
  :defer t
  :config
  (setq expand-region-contract-fast-key "v"))

(use-package helm-mt
  :ensure t
  :defer t)

(use-package linum-relative
  :ensure t
  :defer t
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
    "TODO"
    (unless (eq 'self-insert-command this-command)
      (when linum-schedule-timer
        (cancel-timer linum-schedule-timer))
      (let ((timer (run-with-idle-timer
                    linum-delay nil
                    (lambda ()
                      (setq linum-schedule-timer nil)
                      (linum-update-current)))))
        (setq-local linum-schedule-timer timer))))

  (add-hook 'prog-mode-hook
            (lambda ()
              (set-linum-rel-fmt-for-cur-file)
              (linum-relative-mode)))

  :config
  (setq linum-delay 0.1
        linum-relative-current-symbol ""
        linum-schedule-timer nil)
  (advice-add #'linum-schedule :override #'linum-delay-schedule))

(use-package multi-term
  :ensure t
  :defer t
  :config
  (setq multi-term-program "zsh"))

(use-package osx-dictionary
  :if (eq 'darwin system-type)
  :ensure t
  :defer t)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package server
  :defer t
  :init
  (defun emacs-server-setup ()
    (remove-hook 'focus-out-hook #'emacs-server-setup)
    (require 'server))

  (add-hook 'focus-out-hook #'emacs-server-setup)

  :config
  (unless (server-running-p)
    (server-start)))

(use-package undo-tree
  :ensure t
  :defer t
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist backup-directory-alist))

(use-package vlf-setup
  :ensure vlf)

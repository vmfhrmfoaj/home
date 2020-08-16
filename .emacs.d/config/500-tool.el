;; -*- lexical-binding: t; -*-

(eval-and-compile
  (load-file "~/.emacs.d/config/func.el"))

(add-hook 'after-save-hook #'rsync-remote-dir)
(add-hook 'after-revert-hook #'rsync-remote-dir)
(add-hook 'emacs-startup-hook
          (lambda ()
            "report the startup time."
            (message "Emacs ready in %s(gc: %d times, gc-time: %.3f seconds)."
                     (emacs-init-time)
                     gcs-done
                     gc-elapsed))
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
  :hook ((prog-mode . enable-display-line-numbers))
  :config
  (defun enable-display-line-numbers ()
    "Turn on `display-line-numbers-mode'."
    (let ((buf-name (buffer-name)))
      (when (and (stringp buf-name)
                 (not (string-match-p "^\\s-*\\*" buf-name))
                 (not (minibufferp)))
        (setq display-line-numbers-width
              (max 3 (length (number-to-string (count-lines (point-min) (point-max))))))
        (display-line-numbers-mode)
        (setq display-line-numbers 'visual))))

  (setq-default display-line-numbers-width-start nil))

(use-package ediff
  :defer t
  :config
  (defvar ediff--exclude-mode-status nil
    "TODO")

  (defvar ediff--win-conf nil
    "TODO")

  (defvar ediff--frame-status nil
    "TODO")

  (defun ediff-addtional-setup (&rest _)
    "TODO"
    (setq ediff--exclude-mode-status (-map #'symbol-value ediff-exclude-modes)
          ediff--win-conf (current-window-configuration))
    (disable-modes ediff-exclude-modes)
    (let ((status (frame-parameter nil 'fullscreen)))
      (setq ediff--frame-status status)
      (cond
       ((not status)
        (toggle-frame-maximized))
       ((eq status 'maximized)
        (toggle-frame-fullscreen))
       (t nil))))

  (defun ediff-addtional-cleanup (&rest _)
    "TODO"
    (restore-modes ediff-exclude-modes ediff--exclude-mode-status)
    (-when-let (conf ediff--win-conf)
      (setq ediff--win-conf nil)
      (set-window-configuration conf))
    (cond
     ((not ediff--frame-status)
      (toggle-frame-maximized))
     ((eq ediff--frame-status 'maximized)
      (toggle-frame-fullscreen))
     (t nil))
    (setq ediff--frame-status nil))

  (defun ediff-reset-text-size ()
    "TODO"
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-increase 0))))

  (defun ediff-increase-text-size ()
    "TODO"
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-increase 0.5))))

  (defun ediff-decrease-text-size ()
    "TODO"
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-decrease 0.5))))

  ;; NOTE
  ;;  prevent to calculate the width of the window in `ediff-setup-windows-plain-compare' function.
  (setq ediff-exclude-modes '(zoom-mode)
        ediff-split-window-function #'split-window-right)

  (advice-add #'ediff-setup :before #'ediff-addtional-setup)
  (advice-add #'ediff-quit  :after  #'ediff-addtional-cleanup))

(use-package eldoc
  :defer t
  :config
  (defun eldoc-refresh-for-emacs-27 ()
    (interactive)
    (when (or eldoc-mode
              (and global-eldoc-mode
                   (eldoc--supported-p)))
      (when (timerp eldoc-timer)
        (cancel-timer eldoc-timer)
        (setq eldoc-timer nil))
      (when (and (not (interactive-p))
                 (functionp eldoc-documentation-function))
        (let ((msg (funcall eldoc-documentation-function)))
          (unless (s-blank? msg)
            (eldoc-message msg))))))

  (defun eldoc-refresh-for-emacs-28 (&optional interactive)
    (interactive '(t))
    (when (timerp eldoc-timer)
      (cancel-timer eldoc-timer)
      (setq eldoc-timer nil))
    (let ((token (eldoc--request-state)))
      (cond (interactive
             (setq eldoc--last-request-state token)
             (eldoc--invoke-strategy))
            ((not (eldoc--request-docs-p token))
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc--message nil)))
            (t
             (let ((non-essential t))
               (setq eldoc--last-request-state token)
               ;; Only keep looking for the info as long as the user hasn't
               ;; requested our attention.  This also locally disables
               ;; inhibit-quit.
               (while-no-input
                 (eldoc--invoke-strategy)))))))

  (defalias 'eldoc-refresh
    (if (version<= emacs-version "28.0.50")
        #'eldoc-refresh-for-emacs-28
      #'eldoc-refresh-for-emacs-27))

  (defun eldoc-custom-schedule-timer ()
    "Ensure `eldoc-timer' is running.

If the user has changed `eldoc-idle-delay', update the timer to
reflect the change."
    (when (timerp eldoc-timer)
      (cancel-timer eldoc-timer))
    (setq eldoc-timer
          (run-with-idle-timer
	       eldoc-idle-delay nil
	       (lambda (buf)
             (when (eq (current-buffer) buf)
               (setq eldoc-timer nil)
               (eldoc-refresh)))
           (current-buffer)))

    ;; If user has changed the idle delay, update the timer.
    (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
           (setq eldoc-current-idle-delay eldoc-idle-delay)
           (timer-set-idle-time eldoc-timer eldoc-idle-delay t))))

  (setq eldoc-idle-delay 0.2)

  (add-hook 'eldoc-mode-hook
            (lambda ()
              (eldoc-add-command 'eldoc-refresh)
              (eldoc-refresh))
            :append)

  (advice-add #'eldoc-schedule-timer :override #'eldoc-custom-schedule-timer))

(use-package expand-region
  :ensure t
  :defer t
  :config
  (setq expand-region-contract-fast-key "v"))

(use-package gnuplot
  :ensure t
  :defer t)

(use-package helm-mt
  :ensure t
  :defer t)

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

(use-package strace-mode
  :ensure t
  :defer t)

(use-package tramp
  :defer t
  :config
  (when (file-exists-p (concat home-dir "/.ssh/sockets/"))
    (setq tramp-ssh-controlmaster-options
          (concat "-o ControlMaster=auto "
                  "-o ControlPath='" home-dir "/.ssh/sockets/%%r@%%h-%%p' "
                  "-o ControlPersist=600 "))))

(use-package vlf-setup
  :ensure vlf
  :config
  (defun vlf-custom-beginning-of-file ()
    (interactive)
    (vlf-beginning-of-file)
    (beginning-of-buffer))

  (defun vlf-custom-end-of-file ()
    (interactive)
    (vlf-end-of-file)
    (end-of-buffer))

  (add-hook 'vlf-mode-hook (-partial #'auto-revert-mode -1)))

(use-package ztree
  :ensure t
  :defer t)

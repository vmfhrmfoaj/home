;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(add-hook 'after-save-hook #'rsync-remote-dir)
(add-hook 'after-revert-hook #'rsync-remote-dir)
(add-hook 'emacs-startup-hook
          (lambda ()
            "report the startup time."
            ;; (print features) ; list packages loaded at start up
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
    (unless (frame-focus-state)
      (remove-function after-focus-change-function #'atomic-chrome-setup)
      (require 'atomic-chrome)))

  (if window-system
      ;; NOTE
      ;;  I don't know why `after-focus-change-function' was triggered when starting Emacs.
      ;;  To workaround to adds the setup function to the focus event hook with 0.5 seconds after starting Emacs.
      (add-hook 'after-init-hook
                (lambda ()
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (add-function :after after-focus-change-function #'atomic-chrome-setup)))))
    (require 'atomic-chrome))

  :config
  (atomic-chrome-start-server))

(use-package display-line-numbers
  :hook ((conf-mode            . enable-display-line-numbers)
         (diff-mode            . enable-display-line-numbers)
         (git-timemachine-mode . enable-display-line-numbers)
         (prog-mode            . enable-display-line-numbers)
         (rpm-spec-mode        . enable-display-line-numbers)
         (text-mode            . enable-display-line-numbers))
  :init
  (defun enable-display-line-numbers ()
    "Turn on `display-line-numbers-mode'."
    (when (not (minibufferp))
      (setq display-line-numbers-width
            (max 4 (length (number-to-string (count-lines (point-min) (point-max))))))
      (display-line-numbers-mode)
      (setq display-line-numbers 'visual)))

  (setq-default display-line-numbers-width-start nil))

(use-package ediff
  :defer t
  :config
  (defvar ediff-exclude-modes nil)
  (defvar ediff--exclude-mode-status nil)

  (defvar ediff--win-conf nil)

  (defvar ediff--frame-status nil)

  (defun ediff-addtional-setup (&rest _)
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
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-increase 0))))

  (defun ediff-increase-text-size ()
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-increase 0.5))))

  (defun ediff-decrease-text-size ()
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (dolist (buf (-filter #'identity (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
      (with-current-buffer buf
        (text-scale-decrease 0.5))))

  ;; NOTE
  ;;  prevent to calculate the width of the window in `ediff-setup-windows-plain-compare' function.
  (when (featurep 'zoom-mode)
    (setq ediff-exclude-modes '(zoom-mode)))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain ; for 'Window blur effects' Gnome extension
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
      (when-let ((msg (and (functionp eldoc-documentation-function)
                           (funcall eldoc-documentation-function))))
        (if (interactive-p)
            (eldoc-message msg)
          (when (not (s-blank-str? msg))
            (eldoc-message msg))))))

  (defun eldoc-refresh-for-emacs-28 ()
    (interactive)
    (when (timerp eldoc-timer)
      (cancel-timer eldoc-timer)
      (setq eldoc-timer nil))
    (setq eldoc--last-request-state nil)
    (eldoc--invoke-strategy nil))

  (defalias 'eldoc-refresh
    (if (version<= "28.0.50" emacs-version)
        #'eldoc-refresh-for-emacs-28
      #'eldoc-refresh-for-emacs-27))

  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p max-mini-window-height)

  (add-hook 'eldoc-mode-hook
            (lambda ()
              (eldoc-add-command 'eldoc-refresh)
              (eldoc-refresh))
            :append))

(use-package evil-ex
  :ensure evil
  :defer t
  :config
  (defvar-local evil-ex--gl-preview-point nil
    "TODO")

  (defun evil-ex-update-for--goto-line-preview (&optional beg end len string)
    "TODO"
    ;; (print (list 'env (selected-window) (current-buffer) evil-ex-current-buffer '|
    ;;              'parameters beg end len string '|
    ;;              'variables evil-ex-tree evil-ex-expression evil-ex-range evil-ex-cmd evil-ex-bang evil-ex-argument))
    (when (eq 'evil-goto-line (car evil-ex-expression))
      (-when-let (win (-some->> (window-list)
                        (--filter (eq evil-ex-current-buffer (window-buffer it)))
                        (-first-item)))
        (with-selected-window win
          (unless evil-ex--gl-preview-point
            (setq-local evil-ex--gl-preview-point (point)))
          (let ((line-num (eval (cadadr evil-ex-expression))))
            (goto-line line-num evil-ex-current-buffer)
            (redisplay t)
            (when (bound-and-true-p linum-mode)
              (linum-update evil-ex-current-buffer))
            (when (and (bound-and-true-p hl-line-mode) hl-line-overlay)
              (hl-line-highlight))
            (when (and (bound-and-true-p global-hl-line-mode) global-hl-line-mode)
              (global-hl-line-highlight)))))))

  (defun abort-recursive-edit-for-evil-ex ()
    "TODO"
    (interactive)
    (-when-let (win (-some->> (window-list)
                      (--filter (eq evil-ex-current-buffer (window-buffer it)))
                      (-first-item)))
      (with-selected-window win
        (when evil-ex--gl-preview-point
          (goto-char evil-ex--gl-preview-point))))
    (abort-recursive-edit))

  (advice-add #'evil-ex-setup :before
              (lambda ()
                "setup for `evil-ex-update-for--goto-line-preview' function."
                (with-current-buffer evil-ex-current-buffer
                  (setq-local evil-ex--gl-preview-point nil))))
  (advice-add #'evil-ex-update :after #'evil-ex-update-for--goto-line-preview)
  (advice-add #'evil-ex-execute :before
              (lambda (_)
                "restore the position of the cursor for `evil-ex-update-for--goto-line-preview' function."
                (when evil-ex--gl-preview-point
                  (goto-char evil-ex--gl-preview-point))))

  (setq evil-ex-visual-char-range t))

(use-package expand-region
  :ensure t
  :defer t
  :config
  (setq expand-region-contract-fast-key "v"))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (defun counsel-flycheck--custom-errors-cands ()
    (mapcar
     (lambda (err)
       (propertize
        (format "%s:%s:%s: %s"
                (propertize (file-name-base (flycheck-error-filename err)) 'face 'ivy-grep-info)
                (propertize (int-to-string (flycheck-error-line err)) 'face 'ivy-grep-line-number)
                (let ((level (flycheck-error-level err)))
                  (propertize (symbol-name level) 'face (flycheck-error-level-error-list-face level)))
                (flycheck-error-message err)) 'error err))
     flycheck-current-errors))

  (setq flycheck-display-errors-delay 0.2)

  (add-hook 'flycheck-mode
            (lambda ()
              (add-hook 'company-completion-started-hook
                        (lambda ()
                          (flycheck-stop))
                        nil t)))

  (with-eval-after-load "counsel"
    (advice-add #'counsel-flycheck-errors-cands :override #'counsel-flycheck--custom-errors-cands)))

(use-package flycheck-pos-tip
  :ensure t
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config
  (setq flycheck-pos-tip-timeout 0)

  (add-hook 'evil-normal-state-exit-hook
            (lambda ()
              (setq flycheck-display-errors-function (-const t))))
  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package gnuplot
  :disabled t
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

(use-package pos-tip
  :defer t
  :config
  (defun pos-tip-custom-show (string &optional tip-color pos window timeout width frame-coordinates dx dy)
    (unless window
      (setq window (selected-window)))
    (let ((frame (window-frame window)))
      (unless (cadr (mouse-pixel-position))
        (set-mouse-position frame (- (frame-width) 3) 0))
      (run-with-timer
       0.01 nil (lambda ()
                  (ignore-errors
                    (pos-tip-show-no-propertize
                     string tip-color pos window timeout
                     nil nil frame-coordinates dx dy))))))

  (setq pos-tip-foreground-color (fg-color-from 'default))

  (advice-add #'pos-tip-show :override #'pos-tip-custom-show)
  (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (ignore-errors
                      (pos-tip-hide))))))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package server
  :defer t
  :init
  (defun emacs-server-setup ()
    (remove-function after-focus-change-function #'emacs-server-setup)
    (require 'server))

  (if window-system
      ;; NOTE
      ;;  I don't know why `after-focus-change-function' was triggered when starting Emacs.
      ;;  To workaround to adds the setup function to the focus event hook with 0.5 seconds after starting Emacs.
      (add-hook 'after-init-hook
                (lambda ()
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (add-function :after after-focus-change-function #'emacs-server-setup)))))
    (require 'server))

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (defface treemacs-selected-icon
    '((t :inherit hl-line))
    "TODO")

  (defun treemacs-reset-workspace-and-create-fake ()
    (setq treemacs--workspaces nil
          treemacs-override-workspace (treemacs-workspace->create! :name "Fake"))
    (add-to-list 'treemacs--workspaces treemacs-override-workspace))

  (defun treemacs-current-directory ()
    (interactive)
    (when-let ((file (buffer-file-name)))
      (let ((dir (s-chop-suffix "/" (file-name-directory file))))
        (treemacs-reset-workspace-and-create-fake)
        (treemacs--setup-buffer)
        (treemacs-mode)
        (treemacs-with-writable-buffer
         (let ((treemacs-proj (treemacs-project->create!
                               :name (file-name-nondirectory dir)
                               :path dir
                               :path-status (treemacs--get-path-status dir))))
           (treemacs--add-project-to-current-workspace treemacs-proj)
           (treemacs--add-root-element treemacs-proj))
         (when-let ((pos (next-single-property-change (point-min) :project)))
           (ignore-errors
             (treemacs--expand-root-node pos))
           (goto-char (point-min))
           (when (re-search-forward (regexp-quote (file-name-nondirectory file)) (point-max) nil)
             (beginning-of-line-text)))))))

  (defun treemacs-close-node (&optional arg)
    (interactive "P")
    (-if-let (btn (treemacs-current-button))
        (let ((state (treemacs-button-get btn :state)))
          (cond
           ((string-match-p "open\\(?:-state\\)?$" (symbol-name state))
            (treemacs-TAB-action arg))
           ((string-match-p "\\(node\\|leaf\\|closed\\)\\(?:-state\\)?$" (symbol-name state))
            (treemacs-collapse-parent-node arg))))
      (treemacs-pulse-on-failure "There is nothing to do here.")))

  (defun treemacs-open-node (&optional arg)
    (interactive "P")
    (-if-let (btn (treemacs-current-button))
        (let ((state (treemacs-button-get btn :state)))
          (cond
           ((eq state 'file-node-closed)
            (let ((treemacs-pulse-on-failure nil))
              (unless (treemacs--expand-file-node btn arg)
                (treemacs-RET-action arg))))
           ((string-match-p "closed\\(?:-state\\)?$" (symbol-name state))
            (treemacs-TAB-action arg))
           ((memq state '(tag-node tag-node-leaf))
            (treemacs-RET-action arg))))
      (treemacs-pulse-on-failure "There is nothing to do here.")))

  :config
  (setq treemacs-RET-actions-config
        (let ((visit-fn (lambda (&optional arg)
                          (treemacs-visit-node-default arg)
                          (treemacs-select-window)
                          (treemacs-kill-buffer)))
              (visit-prefer-fn (lambda (&optional arg)
                                 (treemacs-toggle-node-prefer-tag-visit arg)
                                 (treemacs-select-window)
                                 (treemacs-kill-buffer))))
          `((root-node-open   . treemacs-toggle-node)
            (root-node-closed . treemacs-toggle-node)
            (dir-node-open    . treemacs-toggle-node)
            (dir-node-closed  . treemacs-toggle-node)
            (file-node-open   . ,visit-fn)
            (file-node-closed . ,visit-fn)
            (tag-node-open    . ,visit-prefer-fn)
            (tag-node-closed  . ,visit-prefer-fn)
            (tag-node         . ,visit-fn))))

  (treemacs-resize-icons 15)

  (advice-add #'treemacs--setup-icon-background-colors :after
              (lambda (&rest _)
                (when (memq treemacs--selected-icon-background '(unspecified-bg unspecified))
                  (setf treemacs--selected-icon-background (or (face-background 'treemacs-selected-icon nil t)
                                                               treemacs--not-selected-icon-background))
                  (dolist (theme treemacs--themes)
                    (treemacs--maphash (treemacs-theme->gui-icons theme) (_ icon)
                      (treemacs--set-img-property
                       (get-text-property 0 'img-selected icon)
                       :background treemacs--selected-icon-background)
                      (treemacs--set-img-property
                       (get-text-property 0 'img-unselected icon)
                       :background treemacs--not-selected-icon-background)))))))

(use-package treemacs-compatibility
  :after (treemacs winum)
  :config
  (-update->> winum-ignored-buffers-regexp (--remove (string-equal " \\*Treemacs-Scoped-Buffer-" it))))

(use-package treemacs-evil
  :ensure t
  :after treemacs)

(use-package treemacs-projectile
  :ensure t
  :defer t
  :init
  (defun treemacs-projectile-current ()
    (interactive)
    (unless (featurep 'treemacs-projectile)
      (require 'treemacs-projectile))
    (when-let ((proj-root (s-chop-suffix "/" (projectile-project-root))))
      (let ((file (buffer-file-name)))
        (treemacs-reset-workspace-and-create-fake)
        (treemacs--setup-buffer)
        (treemacs-mode)
        (treemacs-with-writable-buffer
         (let ((treemacs-proj (treemacs-project->create!
                               :name (projectile-project-name)
                               :path proj-root
                               :path-status (treemacs--get-path-status proj-root))))
           (treemacs--add-project-to-current-workspace treemacs-proj)
           (treemacs--add-root-element treemacs-proj))
         (when-let ((pos (next-single-property-change (point-min) :project)))
           (ignore-errors
             (treemacs--expand-root-node pos))
           (if (s-blank? file)
               (treemacs-next-line 1)
             (catch 'stop
               (goto-char (point-min))
               (end-of-line) ; for a case that the project name equal to first directory name
               (dolist (part (->> proj-root
                                  (file-relative-name file)
                                  (s-split "/")
                                  (-non-nil)
                                  (--map (concat "\\(?:\\s-\\|/\\)" (regexp-quote it)))))
                 (unless (re-search-forward part (point-max) nil)
                   (throw 'stop))
                 (let ((last (match-end 0)))
                   (beginning-of-line-text)
                   (let ((btn (point)))
                     (when (eq (treemacs-button-get btn :state) 'dir-node-closed)
                       (treemacs--expand-dir-node btn :recursive nil)))
                   (goto-char last))))))))))

  (with-eval-after-load "projectile"
    (require 'treemacs-projectile))
  (with-eval-after-load "treemacs"
    (require 'treemacs-projectile)))

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

(use-package xwidgete
  :ensure t
  :after xwidget)

(use-package ztree
  :ensure t
  :defer t)

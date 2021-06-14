;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'display-line-numbers nil t)
  (require 'ediff nil t)
  (require 'eldoc nil t)
  (require 'evil-ex nil t)
  (require 'expand-region nil t)
  (require 'flycheck nil t)
  (require 'gnuplot nil t)
  (require 'multi-term nil t)
  (require 'saveplace nil t)
  (require 'server nil t)
  (require 'strace-mode nil t)
  (require 'tramp nil t)
  (require 'treemacs nil t)
  (require 'treemacs-compatibility nil t)
  (require 'treemacs-evil nil t)
  (require 'treemacs-projectile nil t)
  (require 'vlf-setup nil t)
  (require 'which-func nil t)
  (require 'xwidgete nil t)
  (require 'ztree nil t))

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

(use-package display-line-numbers
  :hook ((conf-mode            . enable-display-line-numbers)
         (git-timemachine-mode . enable-display-line-numbers)
         (prog-mode            . enable-display-line-numbers)
         (rpm-spec-mode        . enable-display-line-numbers)
         (nxml-mode            . enable-display-line-numbers)
         (yaml-mode            . enable-display-line-numbers))
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
        eldoc-echo-area-use-multiline-p 5)

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

(use-package gitlab-ci-mode
  :ensure t
  :defer t
  :init
  (add-hook 'gitlab-ci-mode-hook
            (lambda ()
              (flycheck-mode 1))))

(use-package gitlab-ci-mode-flycheck
  :ensure t
  :after flycheck
  :config
  (gitlab-ci-mode-flycheck-enable))

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

  (defun wrap-flycheck-display-error-at-point (fn)
    "Wrap `flycheck-display-error-at-point' to display an error message only on user interactive."
    (when (called-interactively-p)
      (funcall fn)))

  (setq flycheck-display-errors-delay 0
        flycheck-flake8-maximum-line-length 120
        flycheck-indication-mode nil)

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          (remove-hook 'after-change-functions #'flycheck-handle-change t)
                          (remove-hook 'post-command-hook #'flycheck-perform-deferred-syntax-check t)
                          (remove-hook 'post-command-hook #'flycheck-error-list-update-source t)
                          (remove-hook 'post-command-hook #'flycheck-error-list-highlight-errors t)
                          (remove-hook 'post-command-hook #'flycheck-maybe-display-error-at-point-soon t)
                          (remove-hook 'post-command-hook #'flycheck-hide-error-buffer t)
                          (flycheck-stop)
                          (flycheck-cancel-error-display-error-at-point-timer)
                          (flycheck--clear-idle-trigger-timer))
                        nil t)
              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          (add-hook 'post-command-hook #'flycheck-hide-error-buffer nil t)
                          (add-hook 'post-command-hook #'flycheck-maybe-display-error-at-point-soon nil t)
                          (add-hook 'post-command-hook #'flycheck-error-list-highlight-errors nil t)
                          (add-hook 'post-command-hook #'flycheck-error-list-update-source nil t)
                          (add-hook 'post-command-hook #'flycheck-perform-deferred-syntax-check nil t)
                          (add-hook 'after-change-functions #'flycheck-handle-change nil t)
                          (flycheck-buffer))
                        nil t)))

  (with-eval-after-load "counsel"
    (advice-add #'counsel-flycheck-errors-cands :override #'counsel-flycheck--custom-errors-cands))

  (advice-add #'flycheck-display-error-at-point :around #'wrap-flycheck-display-error-at-point))

(use-package gnuplot
  :disabled t
  :ensure t
  :defer t)

(use-package multi-term
  :ensure t
  :defer t
  :config
  (setq multi-term-program "zsh"))

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
            (tag-node         . ,visit-fn)))
        treemacs-default-visit-action #'treemacs-visit-node-in-most-recently-used-window
        treemacs-position 'right
        treemacs-width 70)

  (treemacs-resize-icons 13)

  (with-eval-after-load "winum"
    (-update->> winum-ignored-buffers-regexp
                (--remove (s-starts-with? (regexp-quote treemacs--buffer-name-prefix) it))))

  (advice-add #'treemacs--read-string :override #'read-string)
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
  :defer t
  :init
  (defun treemacs-compatibility--undo-for-winum ()
    (remove-hook 'winum-mode-hook #'treemacs-compatibility--undo-for-winum)
    (-update->> winum-ignored-buffers-regexp
                (--remove (string-equal (regexp-quote (format "%sScoped-Buffer-" treemacs--buffer-name-prefix)) it)))
    (fmakunbound #'treemacs-compatibility--undo-for-winum))

  (add-hook 'winum-mode-hook #'treemacs-compatibility--undo-for-winum))

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

(use-package which-func
  :defer t
  :init
  (setq which-func-modes '(emacs-lisp-mode lisp-interaction-mode))

  (defun which-func-setup-once ()
    (remove-hook 'clojure-mode-hook          #'which-func-setup-once)
    (remove-hook 'emacs-lisp-mode-hook       #'which-func-setup-once)
    (remove-hook 'lisp-interaction-mode-hook #'which-func-setup-once)
    (let ((idle-update-delay 0.2))
      (which-function-mode 1)))

  (add-hook 'clojure-mode-hook          #'which-func-setup-once)
  (add-hook 'emacs-lisp-mode-hook       #'which-func-setup-once)
  (add-hook 'lisp-interaction-mode-hook #'which-func-setup-once)

  :config
  (defun which-func-custom-update-1 (window)
    (when (and which-func-mode
               (member major-mode which-func-modes))
      (with-selected-window window
        (condition-case info
	        (let ((current (which-function)))
	          (unless (equal current (gethash window which-func-table))
                (setq spaceline-symbol-segment--symbol current)
                (puthash window current which-func-table)
                (force-mode-line-update)))
	      (error
	       (setq which-func-mode nil)
	       (error "Error in which-func-update: %S" info))))))

  (advice-add #'which-func-update-1 :override #'which-func-custom-update-1))

(use-package xwidgete
  :ensure t
  :after xwidget)

(use-package ztree
  :ensure t
  :defer t)


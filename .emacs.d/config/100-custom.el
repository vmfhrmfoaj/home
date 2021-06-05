;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'func))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(setq auto-revert-interval 1
      auto-save-default nil
      backup-directory-alist `((".*" . "/tmp/"))
      comment-fill-column 100
      create-lockfiles nil
      default-input-method "korean-hangul"
      directory-abbrev-alist '(("/mnt/ext/Archive/"      . "~/Desktop/Archive/")
                               ("/mnt/ext/Contributes/"  . "~/Desktop/Contributes/")
                               ("/mnt/ext/Build/"        . "~/Desktop/Build/")
                               ("/mnt/ext/Open_Sources/" . "~/Desktop/Open_Sources/")
                               ("/mnt/ext/Libraries/"    . "~/Desktop/Libraries/")
                               ("/mnt/ext/Works/"        . "~/Desktop/Works/")
                               ("/mnt/ext2/Downloads/"   . "~/Downloads/"))
      exclude-prev-buf-regex (concat "^\\s-*"
                                     "\\(\\*"
                                     "\\|markdown-code-fontification:"
                                     "\\|magit\\(?:-[a-z]+\\)?:"
                                     "\\)")
      frame-title-format (let ((tail (system-name)))
                           `("%e" (:eval (concat (when-let ((proj-name (and (fboundp 'projectile-project-name)
                                                                            (projectile-project-name))))
                                                   (concat "｢" proj-name "｣ - "))
                                                 ,tail))))
      include-prev-buf-regex (concat "^\\s-*"
                                     "\\(\\*eshell"
                                     "\\|\\*cider-repl "
                                     "\\|\\*emacs-lisp REPL\\*"
                                     "\\|\\*\\s-*docker eshell "
                                     "\\|\\*SQL: "
                                     "\\|\\*Org Agenda"
                                     "\\)")
      scratch-major-mode 'org-mode
      initial-scratch-message ""
      mouse-avoidance-timer (run-with-idle-timer
                             0.1 t
                             (lambda ()
                               ;; NOTE
                               ;;  `frame_make_pointer_invisible' will be called -
                               ;;   only when calling `self-insert-command'.
                               (when (frame-pointer-visible-p)
                                 (with-temp-buffer (self-insert-command 0)))))
      read-process-output-max (* 1024 1024)
      resize-mini-windows t
      ring-bell-function 'ignore
      x-stretch-cursor t)

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t)
  (global-set-key [remap toggle-input-method] #'undefined))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'sql-postgres-login-params 'safe-local-variable (-const t))
(put 'sql-connection-alist      'safe-local-variable (-const t))


(when-let ((input-method (--first (string= "korean-hangul" (car it)) input-method-alist)))
  (setf (nth 3 input-method) "Korean"))

(blink-cursor-mode 0)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'after-init-hook
          (lambda ()
            (add-hook 'buffer-list-update-hook #'update-buf-visit-time)
            (advice-add #'switch-to-buffer :around
                        (lambda (fn buf &rest args)
                          "to prevent duplicated buffer."
                          (let ((buf (get-buffer buf)))
                            (if-let ((win (let ((wins (window-list)))
                                            (when (<= 2 (length wins))
                                              (-some->> wins
                                                        (--filter (-> it (window-buffer) (eq buf)))
                                                        (-first-item))))))
                                (select-window win)
                              (apply fn buf args)))))
            (advice-add #'pop-to-buffer :around
                        (lambda (fn buf &rest args)
                          (let ((buf (get-buffer buf)))
                            (if-let ((win (let ((wins (window-list)))
                                            (when (<= 2 (length wins))
                                              (-some->> wins
                                                        (--filter (-> it (window-buffer) (eq buf)))
                                                        (-first-item))))))
                                (select-window win)
                              (apply fn buf args))))))
          :append)

(add-hook 'kill-emacs-hook
          (lambda ()
            (when-let ((buf (and (stringp scratch-buffer-temp-file)
                                 (get-buffer scratch-buffer-name))))
              (with-current-buffer buf
                (write-region (point-min) (point-max) scratch-buffer-temp-file)))))

(advice-add #'narrow-to-region :after
            (lambda (&rest _)
              "Unselect the region"
              (when (and (called-interactively-p 'interactive)
                         (eq this-command 'narrow-to-region))
                (deactivate-mark))))

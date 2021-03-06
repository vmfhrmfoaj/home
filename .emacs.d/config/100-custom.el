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
  (setq auto-revert-interval 1
        auto-save-default nil
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        backup-directory-alist `((".*" . ,backup-dir))
        backup-inhibited nil
        comment-fill-column 100
        create-lockfiles nil
        default-input-method "korean-hangul"
        directory-abbrev-alist '(("/mnt/ext/Build/"        . "~/Desktop/Build/")
                                 ("/mnt/ext/Open_Sources/" . "~/Desktop/Open_Sources/")
                                 ("/mnt/ext/Libraries/"    . "~/Desktop/Libraries/")
                                 ("/mnt/ext2/Downloads/"   . "~/Downloads/"))
        exclude-prev-buf-regex (concat "^\\s-*"
                                       "\\(\\*"
                                       "\\|markdown-code-fontification:"
                                       "\\|magit\\(?:-[a-z]+\\)?:"
                                       "\\)")
        frame-title-format (let ((tail system-name))
                             `("%e" (:eval (concat (when-let ((proj-name (and (fboundp 'projectile-project-name)
                                                                              (projectile-project-name))))
                                                     (concat "｢" proj-name "｣ - "))
                                                   ,tail))))
        include-prev-buf-regex (concat "^\\s-*\\(?:"
                                       (regexp-opt '("*scratch*" "*emacs-lisp REPL*"))
                                       "\\|\\*eshell\\s-"
                                       "\\|\\*Org"
                                       "\\|\\*cider-repl"
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
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when-let ((input-method (--first (string= "korean-hangul" (car it)) input-method-alist)))
  (setf (nth 3 input-method) "Korean"))

(blink-cursor-mode 0)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

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

(when (eq system-type 'gnu/linux)
  (defun custom-x-dnd-get-drop-x-y (frame w)
    "if top bar is hide, `(frame-parameter frame 'top)' will return a list."
    (let* ((frame-left (frame-parameter frame 'left))
           (frame-left (if (listp frame-left)
                           (eval frame-left)
                         frame-left))
	       (frame-top (frame-parameter frame 'top))
           (frame-top (if (listp frame-top)
                          (eval frame-top)
                        frame-top)))
      (if (windowp w)
	      (let ((edges (window-inside-pixel-edges w)))
	        (cons
	         (+ frame-left (nth 0 edges))
	         (+ frame-top (nth 1 edges))))
        (cons frame-left frame-top))))

  (advice-add #'x-dnd-get-drop-x-y :override #'custom-x-dnd-get-drop-x-y))

(add-hook 'after-save-hook #'garbage-collect 100)
(add-function :after after-focus-change-function
              (lambda ()
                "Reclaim heap memory when fcousing out."
                (unless (frame-focus-state)
                  (garbage-collect)))
              '((depth . 100)))

(advice-add #'narrow-to-region :after
            (lambda (&rest _)
              "unselect the region"
              (when (and (interactive-p)
                         (eq this-command 'narrow-to-region))
                (deactivate-mark))))

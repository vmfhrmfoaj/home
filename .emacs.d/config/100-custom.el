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
        comment-fill-column 100
        create-lockfiles nil
        default-input-method "korean-hangul"
        include-prev-buf-regex (concat "^\\s-*\\(?:"
                                       (regexp-opt '("*scratch*" "*emacs-lisp REPL*")) "\\|"
                                       "\\*eshell\\s-"
                                       "\\)")
        exclude-prev-buf-regex (concat "^\\s-*\\(?:"
                                       "\\*\\|"
                                       "markdown-code-fontification:\\|"
                                       "magit\\(?:-[a-z]+\\)?:"
                                       "\\)")
        initial-major-mode 'text-mode
        initial-scratch-message ""
        read-process-output-max (* 1024 1024)
        ring-bell-function 'ignore))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when-let ((input-method (--first (string= "korean-hangul" (car it)) input-method-alist)))
  (setf (nth 3 input-method) "Korean"))

(blink-cursor-mode 0)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(recentf-mode 1)

(global-subword-mode 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
        mac-pass-control-to-system nil
        mac-pass-command-to-system t)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))

(setq resize-mini-windows t)

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

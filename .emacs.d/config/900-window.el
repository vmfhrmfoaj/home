;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'golden-ratio nil t)
  (require 'winum nil t))

(when window-system
  (toggle-frame-maximized))

(use-package golden-ratio
  :ensure t
  :config
  (defun golden-ratio--custom-resize-window (dimensions &optional window)
    (with-selected-window (or window (selected-window))
      (let* ((m (window-margins))
             (nrow  (floor (- (car  dimensions) (window-height))))
             (ncol  (floor (- (cadr dimensions) (+ (window-width) (or (car m) 0) (or (cdr m) 0))))))
        (when (window-resizable-p (selected-window) nrow)
          (enlarge-window nrow))
        (when (window-resizable-p (selected-window) ncol t)
          (enlarge-window ncol t)))))

  (setq golden-ratio-adjust-factor 1.1)

  (with-eval-after-load "which-key"
    (add-to-list 'golden-ratio-inhibit-functions
                 (lambda ()
                   (and which-key--buffer
                        (window-live-p (get-buffer-window which-key--buffer))))))

  (let ((f (lambda (&rest _) "Run `golden-ratio'" (golden-ratio))))
    (advice-add #'find-file-other-window :after f)

    (with-eval-after-load "winum"
      (advice-add #'winum--switch-to-window :after f))

    (with-eval-after-load "evil"
      (advice-add #'evil-goto-definition :after f))

    (with-eval-after-load "compile"
      (advice-add #'compile-goto-error :after f)))

  (advice-add #'golden-ratio--resize-window :override #'golden-ratio--custom-resize-window)

  (golden-ratio-mode 1))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil
        winum-auto-assign-0-to-minibuffer nil)

  (defun winum-assign-0-to-treemacs ()
    (when (string-match-p "^\\s-*\\*Treemacs-Scoped-Buffer-" (buffer-name))
      0))

  (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)

  (winum-mode))

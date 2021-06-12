;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'golden-ratio nil t)
  (require 'winum nil t))

(when window-system
  (defvar main-monitor
    (let ((get-resolution (lambda (it) (->> it (nth 1) (-take-last 2) (apply #'*)))))
      (-some->> (display-monitor-attributes-list)
                (--max-by (> (funcall get-resolution it)
                             (funcall get-resolution other)))))
    "See `display-monitor-attributes-list'")

  (defcustom title-bar-height-in-pixel 0
    "You set it in `custom-file', rather than here.")

  (let* ((workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-x (nth 0 workarea))
         (main-monitor-y (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (ratio 0.5)
         (wc 120)
         (oc   6) ; fringe + line-number
         (x main-monitor-x)
         (y main-monitor-y)
         (w (+ (* wc (frame-char-width))
               (* oc (frame-char-width))))
         (h (- main-monitor-h title-bar-height-in-pixel)))
    (setq org-tags-column (- wc))
    (if (<= main-monitor-w w)
        (setq x main-monitor-x)
      (setq x (+ (- (+ main-monitor-x (floor (/ main-monitor-w 2)) )
                    (floor (/ w (+ 2 ratio)))))))
    (setq w (- main-monitor-w (- x main-monitor-x)))
    (add-to-list 'default-frame-alist `(width  . (text-pixels . ,w)))
    (add-to-list 'default-frame-alist `(height . (text-pixels . ,h)))
    (setq frame-resize-pixelwise t
          initial-frame-alist `((top  . ,y)
                                (left . ,x))
          split-width-threshold main-monitor-w)))

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
      (advice-add #'evil-goto-definition :after f)))

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

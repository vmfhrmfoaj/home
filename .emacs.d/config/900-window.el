;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'golden-ratio nil t)
  (require 'winum nil t))

(when window-system
  (setq split-width-threshold 999)

  (toggle-frame-maximized)

  (let ((get-resolution (lambda (it) (->> it (nth 1) (-take-last 2) (apply #'*)))))
    (when (<= 1920 (or (-some->> (display-monitor-attributes-list)
                                 (--max-by (> (funcall get-resolution it)
                                              (funcall get-resolution other)))
                                 (assoc 'workarea)
                                 (-drop 1)
                                 (nth 2))
                       0))
      (add-hook 'window-setup-hook
                (lambda ()
                  (split-window-horizontally)
                  (org-agenda-list)
                  (set-window-dedicated-p (selected-window) t)
                  (with-eval-after-load "golden-ratio"
                    (with-selected-window (get-buffer-window org-agenda-buffer)
                      (let ((org-agenda-tags-column (1+ (- (window-text-width)))))
                        (org-agenda-align-tags))))
                  (other-window 1)))

      (with-eval-after-load "org"
        (let ((f (lambda (fn buffer &rest args)
                   "For org-mode dedicated window"
                   (if (and (get-buffer buffer)
                            (window-dedicated-p)
                            (let ((cur-mode major-mode)
                                  (new-mode (with-current-buffer buffer major-mode)))
                              (and (provided-mode-derived-p cur-mode 'org-mode 'org-agenda-mode)
                                   (provided-mode-derived-p new-mode 'org-mode 'org-agenda-mode))))
                       (let* ((win (selected-window))
                              (dedicated-p (window-dedicated-p win)))
                         (unwind-protect
                             (progn
                               (set-window-dedicated-p win nil)
                               (apply fn buffer args))
                           (set-window-dedicated-p win dedicated-p)))
                     (apply fn buffer args)))))
          (advice-add #'pop-to-buffer-same-window :around f)
          (advice-add #'switch-to-buffer :around f))))))

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

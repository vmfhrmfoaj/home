;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(when window-system
  (let* ((workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-x (nth 0 workarea))
         (main-monitor-y (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (x main-monitor-x)
         (y main-monitor-y)
         (w main-monitor-w)
         (h main-monitor-h))
    (setq w (- main-monitor-w (- x main-monitor-x)))
    (add-to-list 'default-frame-alist `(width  . (text-pixels . ,w)))
    (add-to-list 'default-frame-alist `(height . (text-pixels . ,h)))
    (setq frame-resize-pixelwise t
          initial-frame-alist `((top  . ,y)
                                (left . ,x)
                                (undecorated . nil))
          split-width-threshold main-monitor-w)
    (toggle-frame-maximized)))

(add-hook 'window-setup-hook
          (lambda ()
            (let ((buf (current-buffer)))
              (org-agenda-show-list)
              (split-window-horizontally)
              (set-window-dedicated-p (selected-window) t)
              (other-window 1)
              (switch-to-buffer buf))))

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
    (advice-add #'switch-to-buffer :around f)))

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

  (setq golden-ratio-adjust-factor 1.05)

  (with-eval-after-load "which-key"
    (add-to-list 'golden-ratio-exclude-buffer-names which-key-buffer-name))

  (with-eval-after-load "winum"
    (advice-add #'winum--switch-to-window :after (lambda (&rest _) (golden-ratio))))

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

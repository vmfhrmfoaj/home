;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(when window-system
  (let* ((workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-x (nth 0 workarea))
         (main-monitor-y (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (x-offset (- (frame-char-width)))
         (x main-monitor-x)
         (y main-monitor-y)
         (h main-monitor-h)
         (wc 120)
         (oc   6) ; fringe + line-number
         (w (+ (* wc (frame-char-width))
               (* oc (frame-char-width)))))
    (if (<= main-monitor-w w)
        (setq w main-monitor-w)
      (setq x (+ (- (+ main-monitor-x
                       (floor (/ main-monitor-w 2)) )
                    (floor (/ w 2)))
                 x-offset)))
    (add-to-list 'default-frame-alist `(width  . (text-pixels . ,w)))
    (add-to-list 'default-frame-alist `(height . (text-pixels . ,h)))
    (setq frame-resize-pixelwise t
          initial-frame-alist (list `(top  . ,y)
                                    `(left . ,x)
                                    '(undecorated . nil))
          split-width-threshold main-monitor-w)))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil
        winum-auto-assign-0-to-minibuffer nil)

  (defun winum-assign-0-to-treemacs ()
    (when (string-match-p "^\\s-*\\*Treemacs-Scoped-Buffer-" (buffer-name)) 0))

  (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)
  (winum-mode))

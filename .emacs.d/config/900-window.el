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
         (x-offset 200)
         (x main-monitor-x)
         (y main-monitor-y)
         (h main-monitor-h)
         (wc 120)
         (oc   5) ; fringe + line-number
         (w (+ (* wc (frame-char-width))
               (* oc (frame-char-width))))
         (cur (selected-frame)))
    (setq org-tags-column (- wc))
    (if (<= main-monitor-w w)
        (setq x main-monitor-x)
      (setq x (+ (- (+ main-monitor-x
                       (floor (/ main-monitor-w 2)) )
                    (floor (/ w 2)))
                 x-offset)))
    (setq w (- main-monitor-w (- x main-monitor-x)))
    (add-to-list 'default-frame-alist `(width  . (text-pixels . ,w)))
    (add-to-list 'default-frame-alist `(height . (text-pixels . ,h)))
    (setq frame-resize-pixelwise t
          initial-frame-alist (list `(top  . ,y)
                                    `(left . ,x)
                                    '(undecorated . nil))
          split-width-threshold main-monitor-w
          sidebar-title "Sidebar"
          sidebar-w (- x main-monitor-x (* oc (frame-char-width)))
          sidebar--width-change-timer nil)
    (add-hook
     'window-setup-hook
     (-partial #'make-thread
               (lambda ()
                 (let ((w sidebar-w))
                   (setq sidebar-frame (make-frame `((sig    . ,sidebar-title)
                                                     (width  . (text-pixels . ,w))
                                                     (height . (text-pixels . ,h)))))
                   (set-frame-position sidebar-frame main-monitor-x main-monitor-y)
                   (sit-for 0.05)
                   (set-frame-size sidebar-frame w h t)
                   (sit-for 0.05)
                   (x-focus-frame cur)
                   (with-selected-frame sidebar-frame
                     (ignore-errors
                       (org-agenda-show-list)
                       (pop-to-scratch-buffer)))))))))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil
        winum-auto-assign-0-to-minibuffer nil)

  (defun winum-assign-0-to-treemacs ()
    (when (string-match-p "^\\s-*\\*Treemacs-Scoped-Buffer-" (buffer-name))
      0))

  (defun winum-assign-9-to-treemacs ()
    (when (and (string-equal sidebar-title (frame-parameter nil 'sig))
               (not (ignore-errors
                      (aref (winum--get-window-vector) 9))))
      9))

  (defun winum-assign-8-to-treemacs ()
    (when (and (string-equal sidebar-title (frame-parameter nil 'sig))
               (string-equal scratch-buffer-name (buffer-name)))
      8))

  (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)
  (add-to-list 'winum-assign-functions #'winum-assign-8-to-treemacs)
  (add-to-list 'winum-assign-functions #'winum-assign-9-to-treemacs)
  (winum-mode))

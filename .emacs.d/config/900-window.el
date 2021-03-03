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
         (x-offset 190)
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
          sidebar-title "Sidebar")
    (add-hook 'window-setup-hook
              (lambda ()
                (let ((w (- x main-monitor-x (* oc (frame-char-width)))))
                  (setq sidebar-frame (make-frame `((name . ,sidebar-title))))
                  (set-frame-position sidebar-frame main-monitor-x main-monitor-y)
                  (set-frame-size     sidebar-frame w h t)
                  (with-selected-frame sidebar-frame
                    (org-agenda-show-list))
                  (x-focus-frame cur))))))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil
        winum-auto-assign-0-to-minibuffer nil)

  (defun winum-assign-0-to-treemacs ()
    (when (string-match-p "^\\s-*\\*Treemacs-Scoped-Buffer-" (buffer-name)) 0))

  (defun winum-assign-9-to-treemacs ()
    (when (string-equal sidebar-title (frame-parameter nil 'name)) 9))

  (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)
  (add-to-list 'winum-assign-functions #'winum-assign-9-to-treemacs)
  (winum-mode))

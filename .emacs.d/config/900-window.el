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
              (other-window 1)
              (switch-to-buffer buf))))

(let ((f (lambda (fn buffer &rest args)
           (let ((cur-mode major-mode)
                 (new-mode (with-current-buffer buffer major-mode)))
             (if (and (window-dedicated-p)
                      (provided-mode-derived-p cur-mode 'org-mode 'org-agenda-mode)
                      (provided-mode-derived-p new-mode 'org-mode 'org-agenda-mode))
                 (let ((win (selected-window)))
                   (unwind-protect
                       (progn
                         (set-window-dedicated-p win nil)
                         (switch-to-buffer buffer))
                     (set-window-dedicated-p win t)))
               (apply fn buffer args))))))
  (advice-add #'pop-to-buffer-same-window :around f)
  (advice-add #'switch-to-buffer :around f))

(use-package golden-ratio
  :ensure t
  :config
  (with-eval-after-load "winum"
    (advice-add #'winum--switch-to-window :after (lambda (&rest _) (golden-ratio))))

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

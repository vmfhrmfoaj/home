;; -*- lexical-binding: t; -*-

(eval-and-compile
  (load-file "~/.emacs.d/config/func.el"))

(when window-system
  (let* ((min-w (* 140 (frame-char-width)))
         (ratio 0.23)
         (workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-x (nth 0 workarea))
         (main-monitor-y (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (x main-monitor-x)
         (y main-monitor-y)
         (w main-monitor-w)
         (h main-monitor-h))
    (if (<= main-monitor-w min-w)
        (set-frame-parameter nil 'fullscreen 'maximized)
      (setq x (+ main-monitor-x (floor (* main-monitor-w ratio)))
            w (- main-monitor-w x))
      (when (< w min-w)
        (setq w min-w
              x (+ main-monitor-x (- main-monitor-w min-w)))))
    (add-to-list 'default-frame-alist `(width  . (text-pixels . ,w)))
    (add-to-list 'default-frame-alist `(height . (text-pixels . ,h)))
    (setq frame-resize-pixelwise t
          initial-frame-alist (list `(top  . ,y)
                                    `(left . ,x)
                                    '(undecorated . t))
          split-width-threshold main-monitor-w)))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil)

  (winum-mode))

(use-package zoom
  :disabled t
  :ensure t
  :defer t
  :init
  (defun zoom-initial-setup ()
    (remove-hook 'window-configuration-change-hook #'zoom-initial-setup)
    (require 'zoom))

  (add-hook 'after-init-hook
            (lambda ()
             (add-hook 'window-configuration-change-hook #'zoom-initial-setup)))

  :config
  (defun zoom--handler-wrapper-for-helm (f &optional ignored)
    (unless (helm--alive-p)
      (funcall f ignored)))

  (defun zoom--update-for-helm ()
    "Update the window layout in the current frame. (custom ver)"
    (let ((zoom-mode nil)
          (window-configuration-change-hook nil)
          (window-combination-resize t)
          (window-resize-pixelwise t))
      (unless (and (bound-and-true-p helm-autoresize-mode)
                   (fboundp #'helm--alive-p)
                   (helm--alive-p))
        (balance-windows))
      (unless (zoom--window-ignored-p)
        (zoom--resize)
        (zoom--fix-scroll))))

  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("\\*.*[Hh]elm.*\\*"))

  (advice-add #'zoom--handler :around #'zoom--handler-wrapper-for-helm)
  (advice-add #'zoom--update :override #'zoom--update-for-helm)

  (zoom-mode t))

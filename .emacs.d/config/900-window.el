(when window-system
  (let* ((min-col-w 140)
         (min-col-pixel (* min-col-w (frame-char-width)))
         (workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-x (nth 0 workarea))
         (main-monitor-y (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (x main-monitor-x)
         (w (/ main-monitor-w (frame-char-width)))
         (h (/ main-monitor-h (frame-char-height))))
    (if (<= main-monitor-w min-col-pixel)
        (set-frame-parameter nil 'fullscreen 'maximized)
      (setq x (+ main-monitor-x (floor (* main-monitor-w 0.21)))
            w (/ (- main-monitor-w x) (frame-char-width)))
      (when (< w min-col-w)
        (setq w min-col-w
              x (+ main-monitor-x (- main-monitor-w min-col-pixel)))))
    (add-to-list 'default-frame-alist (cons 'width  w))
    (add-to-list 'default-frame-alist (cons 'height h))
    (setq split-width-threshold main-monitor-w
          initial-frame-alist (list (cons 'top    main-monitor-y)
                                    (cons 'left   x)
                                    (cons 'width  w)
                                    (cons 'height h)
                                    (cons 'undecorated t)))))

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil)

  (winum-mode))

(use-package zoom
  :ensure t
  :defer t
  :init
  (defn zoom-initial-setup ()
    (remove-hook 'window-configuration-change-hook #'zoom-initial-setup)
    (require 'zoom))

  (add-hook 'after-init-hook
            (lambda ()
             (add-hook 'window-configuration-change-hook #'zoom-initial-setup)))

  :config
  (defn zoom--handler-wrapper-for-helm (f &optional ignored)
    (unless (helm--alive-p)
      (funcall f ignored)))

  (defn zoom--update-for-helm ()
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

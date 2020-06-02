(when window-system
  (let* ((two-horizontal-window-setup nil)
         (min-col-width 170)
         (workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-l (nth 0 workarea))
         (main-monitor-t (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea))
         (width main-monitor-w)
         (heigh main-monitor-h)
         (x-pos 0))
    (if (or two-horizontal-window-setup
            (<= main-monitor-w (* min-col-width (frame-char-width))))
        (set-frame-parameter nil 'fullscreen 'maximized)
      (set-frame-position (selected-frame) main-monitor-l main-monitor-t)
      (let* ((h (floor (/ (float main-monitor-h) (frame-char-height))))
             (l (/ main-monitor-w 2.0))
             (l (floor (- l (* (frame-unit->pixel min-col-width) 0.4))))
             (l (if (< 0 (- main-monitor-w (+ l (frame-unit->pixel min-col-width))))
                    l
                  (max 0 (- main-monitor-w (frame-unit->pixel min-col-width)))))
             (w (max min-col-width (/ (- main-monitor-w l) (frame-char-width)))))
        (setq width w
              heigh h
              x-pos (+ l main-monitor-l))
        (add-to-list 'default-frame-alist (cons 'width  w))
        (add-to-list 'default-frame-alist (cons 'height h))))
    (setq split-width-threshold (if two-horizontal-window-setup
                                    min-col-width
                                  main-monitor-w)
          initial-frame-alist (list (cons 'top    main-monitor-t)
                                    (cons 'left   x-pos)
                                    (cons 'width  width)
                                    (cons 'height heigh)
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

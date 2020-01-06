(when window-system
  (let* ((min-col-width 140)
         (workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-l (nth 0 workarea))
         (main-monitor-t (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea)))
    (if (<= main-monitor-w (* min-col-width (frame-char-width)))
        (set-frame-parameter nil 'fullscreen 'maximized)
      (set-frame-position (selected-frame) main-monitor-l main-monitor-t)
      (let* ((h (floor (/ (float main-monitor-h) (frame-char-height))))
             (l (/ main-monitor-w 2.0))
             (l (floor (- l (* (frame-unit->pixel min-col-width) 0.4))))
             (l (if (< 0 (- main-monitor-w (+ l (frame-unit->pixel min-col-width))))
                    l
                  (max 0 (- main-monitor-w (frame-unit->pixel min-col-width)))))
             (w (max min-col-width (/ (- main-monitor-w l) (frame-char-width)))))
        (add-to-list 'default-frame-alist (cons 'width  w))
        (add-to-list 'default-frame-alist (cons 'height h))
        (setq split-width-threshold main-monitor-w
              initial-frame-alist (list (cons 'top    main-monitor-t)
                                        (cons 'left   (+ l main-monitor-l))
                                        (cons 'width  w)
                                        (cons 'height h)))))))

(use-package winum
  :ensure t
  :config
  (defn winum--assign-0-to-neotree ()
    "winum assign function for NeoTree."
    (when (and (string-match-p "\\*NeoTree\\*" (buffer-name))
               (not (aref (winum--get-window-vector) 0)))
      0))

  (setq winum-auto-setup-mode-line nil)
  (add-to-list 'winum-assign-functions #'winum--assign-0-to-neotree)

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
      ;; NOTE
      ;;  temporary fix
      ;;  In MELPA, yet, a patch is not applied.
      (unless (or (equal (selected-window) zoom--last-window)
                  (and zoom-minibuffer-preserve-layout (window-minibuffer-p))
                  track-mouse)
        (setq zoom--last-window (selected-window)))
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

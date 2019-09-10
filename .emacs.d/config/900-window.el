(when window-system
  (let* ((column-width 160)
         (workarea (-some->> main-monitor (assoc 'workarea) (-drop 1)))
         (main-monitor-l (nth 0 workarea))
         (main-monitor-t (nth 1 workarea))
         (main-monitor-w (nth 2 workarea))
         (main-monitor-h (nth 3 workarea)))
    ;; NOTE
    ;;  On Xming, `workarea' and `geometry' are always same.
    (when (equal workarea (-some->> main-monitor (assoc 'geometry) (-drop 1)))
      ;; for WSL (Windows Linux Subsystem)
      (let ((title-bar-h 31)
            (task-bar-h 44))
        (setq main-monitor-t (+ main-monitor-t title-bar-h))
        (setq main-monitor-h (- main-monitor-h title-bar-h task-bar-h))))
    (if (<= main-monitor-w (* column-width (frame-char-width)))
        (set-frame-parameter nil 'fullscreen 'maximized)
      (set-frame-position (selected-frame) main-monitor-l main-monitor-t)
      (let* ((h (floor (/ (float main-monitor-h) (frame-char-height))))
             (l (/ main-monitor-w 2.0))
             (l (floor (- l (* (frame-unit->pixel column-width) 0.4))))
             (l (if (< 0 (- main-monitor-w (+ l (frame-unit->pixel column-width))))
                    l
                  (max 0 (- main-monitor-w (frame-unit->pixel column-width)))))
             (w (max column-width (/ (- main-monitor-w l) (frame-char-width)))))
        (add-to-list 'default-frame-alist (cons 'width  w))
        (add-to-list 'default-frame-alist (cons 'height h))
        (setq split-width-threshold main-monitor-w
              initial-frame-alist (list (cons 'top    main-monitor-t)
                                        (cons 'left   (+ l main-monitor-l))
                                        (cons 'width  w)
                                        (cons 'height h)))))))

(unless window-system
  (menu-bar-mode -1))

(use-package winum
  :ensure t
  :init
  (defn winum--assign-0-to-neotree ()
    "winum assign function for NeoTree."
    (when (and (string-match-p "\\*NeoTree\\*" (buffer-name))
               (not (aref (winum--get-window-vector) 0)))
      0))

  :config
  (setq winum-auto-setup-mode-line nil)
  (add-to-list 'winum-assign-functions #'winum--assign-0-to-neotree)
  (winum-mode))

(use-package zoom
  :ensure t
  :init
  (defn zoom--handler-wrapper-for-helm (f &optional ignored)
    (unless (and (fboundp #'helm--alive-p)
                 (helm--alive-p))
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

  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("\\*.*[Hh]elm.*\\*"))
  (advice-add #'zoom--handler :around #'zoom--handler-wrapper-for-helm)
  (advice-add #'zoom--update :override #'zoom--update-for-helm)
  (zoom-mode t))

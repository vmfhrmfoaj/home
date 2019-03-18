(let ((w 142)
      (h-char (frame-char-height))
      (h-titlebar-in-char 2)
      (top-offset (if (string-equal "gnome-imac" hostname) 5 0)))
  (setq org-tags-column (+ (- w) 5))
  (when window-system
    (if (<= (display-pixel-width) 1440)
        (set-frame-parameter nil 'fullscreen 'maximized)
      (let* ((h (floor (- (/ (float (display-pixel-height)) h-char) h-titlebar-in-char)))
             (l (/ (custom-display-pixel-width) 2.0))
             (l (floor (- l (* (frame-unit->pixel w) 0.4))))
             (l (if (< 0 (- (custom-display-pixel-width)
                            (+ l (frame-unit->pixel w))))
                    l
                  (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
        (add-to-list 'default-frame-alist (cons 'width  w))
        (add-to-list 'default-frame-alist (cons 'height h))
        (setq split-width-threshold (display-pixel-width)
              initial-frame-alist (list (cons 'top    0)
                                        (cons 'left   l)
                                        (cons 'width  w)
                                        (cons 'height h))
              frame-title-format
              `(multiple-frames "%b" ("" invocation-name "@" system-name
                                      ,(-reduce #'concat (-repeat (pixel->frame-unit l) " ")))))))))

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
  (defn zoom--handler-for-helm (&optional window-or-frame norecord)
    (unless (bound-and-true-p helm-alive-p)
      (zoom--handler window-or-frame norecord)))

  (defn zoom--on-for-helm ()
    "TODO"
    ;; register the zoom handler
    ;; NOTE
    ;;  It confilct `helm-display-buffer-at-bottom'
    (add-hook 'window-size-change-functions #'zoom--handler-for-helm)
    ;; (add-hook 'minibuffer-setup-hook #'zoom--handler)
    (advice-add #'select-window :after #'zoom--handler)
    ;; disable mouse resizing
    (advice-add #'mouse-drag-mode-line :override #'ignore)
    (advice-add #'mouse-drag-vertical-line :override #'ignore)
    (advice-add #'mouse-drag-header-line :override #'ignore)
    ;; update the layout once loaded
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (zoom--handler))))

  (defn zoom--update-for-helm ()
    "Update the window layout in the current frame. (custom ver)"
    (let ((zoom-mode nil)
          (window-configuration-change-hook nil)
          (window-combination-resize t)
          (window-resize-pixelwise t))
      ;; NOTE
      ;;  It confilct `helm-autoresize-mode'
      ;; (balance-windows)
      (unless (zoom--window-ignored-p)
        (balance-windows)
        (zoom--resize)
        (zoom--fix-scroll))))

  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("\\*.*[Hh]elm.*\\*"))
  (advice-add #'zoom--on     :override #'zoom--on-for-helm)
  (advice-add #'zoom--update :override #'zoom--update-for-helm)
  (zoom-mode t))

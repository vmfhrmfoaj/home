(let ((w 160)
      (h-char (frame-char-height))
      (h-titlebar-in-char 2)
      (top-offset (if (string-equal "gnome-imac" hostname) 5 0)))
  (setq org-tags-column (+ (- w) 5))
  (when window-system
    (if (<= (display-pixel-width) (* w (frame-char-width)))
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

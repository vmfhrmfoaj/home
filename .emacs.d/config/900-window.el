(tool-bar-mode   -1)
(scroll-bar-mode -1)

(when window-system
  (let ((w 150))
    (setq org-tags-column (+ (- w) 5))
    (let* ((h (- (/ (display-pixel-height) (frame-char-height))
                 (if (zerop (% (display-pixel-height) (frame-char-height)))
                     2 1)
                 2))
           (l (/ (custom-display-pixel-width) 2.0))
           (l (floor (- l (* (frame-unit->pixel w) 0.45))))
           (l (if (< 0 (- (custom-display-pixel-width)
                          (+ l (frame-unit->pixel w))))
                  l
                (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
      (add-to-list 'default-frame-alist (cons 'width  w))
      (add-to-list 'default-frame-alist (cons 'height h))
      (setq split-width-threshold (1+ w)
            initial-frame-alist (list (cons 'top    (1+ (frame-char-height)))
                                      (cons 'left   l)
                                      (cons 'width  w)
                                      (cons 'height h))))))

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package zoom
  :ensure t
  :init
  (defun zoom--update-custom ()
    "Update the window layout in the current frame. (custom ver)"
    (let ((zoom-mode nil)
          (window-configuration-change-hook nil)
          (window-combination-resize t)
          (window-resize-pixelwise t))
      (unless (zoom--window-ignored-p)
        (balance-windows)
        (zoom--resize)
        (zoom--fix-scroll))))

  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
  (advice-add #'zoom--update :override #'zoom--update-custom)
  (zoom-mode t))

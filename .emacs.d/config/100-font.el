(when window-system
  (let* ((font "DejaVu Sans Mono")
         (spacing 0)
         (height (cond
                  ((equal '(1440  900) main-monitor-resolution) 113)
                  ((equal '(1920 1080) main-monitor-resolution) 105))))
    (when height
      (setq font-height height))
    (setq-default line-spacing spacing)
    (set-face-font 'default font)
    (set-fontset-font t 'unicode "symbola"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("DejaVu Serif"     . 0.95))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.95))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.9))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.85))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.85)))

(prefer-coding-system 'utf-8)

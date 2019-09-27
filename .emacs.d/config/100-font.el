(when window-system
  (let* ((font "Monoid")
         (spacing 0)
         (height (cond
                  ((equal '(1440  900) main-monitor-resolution) 90)
                  ((equal '(1920 1080) main-monitor-resolution) 90)
                  (t 90))))
    (defvar font-height
      height
      "Default font heigh.")
    (setq-default line-spacing spacing)
    (set-face-font 'default font)
    (set-fontset-font t 'unicode "symbola"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.95))
  (add-to-list 'face-font-rescale-alist '("Noto Sans CJK JP" . 0.9))
  (add-to-list 'face-font-rescale-alist '("Noto Sans CJK KR" . 0.9))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.95))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.9))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.85))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.85)))

(prefer-coding-system 'utf-8)

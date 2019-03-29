(when (window-system)
  (setq-default line-spacing
                (cond
                 ((string-equal "gnome-imac" hostname) 7)
                 (t 6)))
  (let ((font "DejaVu Sans Mono")
        (height (cond
                 ((string-equal "gnome-macbookair" hostname) 113)
                 ((string-equal "gnome-imac" hostname) 109) ; for HiDPI=2 and Scaling Factor=0.9
                 (t 109))))
    (set-face-font 'default font)
    (set-fontset-font t 'unicode "symbola")
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height))
  (set-fontset-font t 'hangul (font-spec :name "Noto Sans CJK KR"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Free-Symbola"     . 0.9))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.95))
  (add-to-list 'face-font-rescale-alist '("Unifont"          . 1.0))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.85))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.85))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.85)))

(prefer-coding-system 'utf-8)

(when (window-system)
  (setq-default line-spacing 6)
  (let ((font "Inconsolata")
        (height (cond
                 ((string-equal "gnome-macbookair" hostname) 128)
                 ((string-equal "gnome-imac" hostname) 125) ; for HiDPI=2 and Scaling Factor=0.9
                 (t 128))))
    (set-face-font 'default font)
    (set-fontset-font t 'unicode "symbola")
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height))
  (set-fontset-font t 'hangul (font-spec :name "Noto Sans CJK KR"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.9))
  (add-to-list 'face-font-rescale-alist '("DejaVu Serif"     . 0.9))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.85))
  (add-to-list 'face-font-rescale-alist '("Free-Symbola"     . 0.85))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.9))
  (add-to-list 'face-font-rescale-alist '("Noto Sans CJK KR" . 0.8))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Unifont"          . 0.95))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.9))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.8))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.8))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.8)))

(prefer-coding-system 'utf-8)

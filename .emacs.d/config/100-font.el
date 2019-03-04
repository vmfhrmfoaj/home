(when (window-system)
  (setq-default line-spacing 5)
  (let ((font "Noto Sans Mono")
        (height (cond
                 ((string-equal "gnome-macbookair" hostname) 113)
                 ((string-equal "gnome-imac" hostname) 109) ; for HiDPI=2 and Scaling Factor=0.9
                 (t 105))))
    (set-face-font 'default font)
    (set-fontset-font t 'unicode "symbola")
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height))
  (set-fontset-font t 'hangul (font-spec :name "Noto Sans Mono CJK KR"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("DejaVu Serif"     . 0.95))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.95))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.85))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.85))
  (when (or (string-equal "gnome-macbookair" hostname)
            (string-equal "gnome-imac" hostname))
    (add-to-list 'face-font-rescale-alist '("Free-Symbola" . 0.95))))

(prefer-coding-system 'utf-8)

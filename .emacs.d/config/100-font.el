(when window-system
  (let* ((font "Fantasque Sans Mono")
         (spacing 1)
         (height 120))
    (setq-default line-spacing spacing)
    (set-face-font 'default font)
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("DejaVu Serif"     . 0.95))
  (add-to-list 'face-font-rescale-alist '("FontAwesome"      . 0.9))
  (add-to-list 'face-font-rescale-alist '("Noto"             . 0.9))
  (add-to-list 'face-font-rescale-alist '("Material Icons"   . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral"      . 0.95))
  (add-to-list 'face-font-rescale-alist '("Weather Icons"    . 0.95))
  (add-to-list 'face-font-rescale-alist '("all-the-icons"    . 0.9))
  (add-to-list 'face-font-rescale-alist '("file-icons"       . 0.8))
  (add-to-list 'face-font-rescale-alist '("github-octicons"  . 0.85)))

(prefer-coding-system 'utf-8)
(set-fontset-font t 'unicode "symbola")

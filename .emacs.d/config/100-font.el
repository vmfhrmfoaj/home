(when (window-system)
  (setq-default line-spacing 0)
  ;; In Linux the font hinting must not be 'full', it causes the bold font make smaller.
  ;; Options(only available on macOS):
  ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 0~3
  ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 0~16
  ;; Options(x11):
  ;; - .Xresource:
  ;;   Xft.embolden: true
  ;;   Emacs.fontBackend: xft
  (let ((font "Fantasque Sans Mono")
        (height (cond
                 ((string-equal "gnome-macbookair" hostname) 110)
                 ((string-equal "gnome-imac" hostname) 105) ; for HiDPI=2 and Scaling Factor=0.9
                 (t 102))))
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

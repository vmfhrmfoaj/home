(when (window-system)
  (setq-default line-spacing 0)
  ;; Options(only available on macOS):
  ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 0~3
  ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 0~16
  ;; In Linux the font hinting must not be 'full', it causes the bold font make smaller.
  (let ((font "MonacoB2")
        (height (if (eq 'darwin system-type) 141 90)))
    (set-face-font 'default font)
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height))
  (set-fontset-font t 'hangul (font-spec :name "Nanum Gothic"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Free-Symbola" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral" . 0.9))
  (when (eq 'gnu/linux system-type)
    (add-to-list 'face-font-rescale-alist '("Fira Code Symbol". 0.95))
    (add-to-list 'face-font-rescale-alist '("FontAwesome" . 0.95))
    (add-to-list 'face-font-rescale-alist '("all-the-icons" . 0.9))
    (add-to-list 'face-font-rescale-alist '("file-icons" . 0.9))))

;; hangul
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

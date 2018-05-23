(when (window-system)
  (setq-default line-spacing 1)
  (let* ((mac? (eq 'darwin system-type))
         (font "MonacoB")
         (height (if mac? 151 105)))
    (set-face-font 'default font)
    (set-face-attribute 'default nil
                        :weight 'bold
                        :width 'medium
                        :height height))
  (set-fontset-font t 'hangul (font-spec :name "Nanum Gothic"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Free-Symbola" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral" . 0.9)))

;; hangul
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

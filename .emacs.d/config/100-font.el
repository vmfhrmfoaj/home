(when (window-system)
  (setq-default line-spacing 0)
  (let ((font "MonacoB")
        (height (if (eq 'darwin system-type) 141 90)))
    (set-face-font 'default font)
    (set-face-attribute 'default nil
                        :weight 'bold
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

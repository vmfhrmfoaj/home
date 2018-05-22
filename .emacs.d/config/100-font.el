(when (window-system)
  (setq-default line-spacing 1)
  (set-face-font 'default "MonacoB-15")
  (set-face-attribute 'default nil :weight 'bold)
  (set-fontset-font t 'hangul (font-spec :name "Nanum Gothic"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral" . 0.9))
  (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95)))

;; hangul
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

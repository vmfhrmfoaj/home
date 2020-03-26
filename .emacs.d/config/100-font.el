(when window-system
  (setq-default line-spacing 1)
  (set-face-font 'default
                 (font-spec :family "Noto Sans Mono"
                            :size 15)))

(make-thread (lambda ()
               (prefer-coding-system 'utf-8)
               (set-fontset-font t 'hangul "Noto Sans Mono CJK KR")
               (when window-system
                 (add-to-list 'face-font-rescale-alist '("Arial Unicode MS"   . 0.9))
                 (add-to-list 'face-font-rescale-alist '("DejaVu Sans"        . 0.9))
                 (add-to-list 'face-font-rescale-alist '("DejaVu Serif"       . 0.9))
                 (add-to-list 'face-font-rescale-alist '("Free-Symbola"       . 0.85))
                 (add-to-list 'face-font-rescale-alist '("Noto Sans CJK"      . 0.9))
                 (add-to-list 'face-font-rescale-alist '("Noto Sans Mono CJK" . 0.95))
                 (add-to-list 'face-font-rescale-alist '("STIXGeneral"        . 0.9))
                 (add-to-list 'face-font-rescale-alist '("Source Han Sans"    . 0.85))
                 (add-to-list 'face-font-rescale-alist '("Unifont"            . 0.95)))))

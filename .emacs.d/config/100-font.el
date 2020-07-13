(defvar default-font
  (font-spec :family "Cascadia Code" :size 16)
  "TODO")

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 1)
  (set-face-font 'default default-font)
  (set-fontset-font t '(#xE000 . #xF8FF) default-font)                 ; for ligature
  (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) default-font) ; for ligature
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Color Emoji" :size 13))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans" :size 14) nil t)
  (set-fontset-font "fontset-default" 'hangul  (font-spec :family "Noto Sans CJK KR" :size 14))
  (add-to-list 'face-font-rescale-alist '("DejaVu Sans Mono" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Noto Sans Mono"   . 0.85)))

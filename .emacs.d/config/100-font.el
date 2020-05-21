(when window-system
  (prefer-coding-system 'utf-8)
  (setq use-default-font-for-symbols nil)
  (setq-default line-spacing 1)
  (set-face-font 'default (font-spec :family "Fantasque Sans Mono" :size 16))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Sans CJK KR" :size 12))
  (set-fontset-font "fontset-default" 'hangul  (font-spec :family "Noto Sans CJK KR" :size 14)))

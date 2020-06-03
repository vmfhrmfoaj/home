(defvar default-font
  (font-spec :family "Fantasque Sans Mono" :size 16)
  "TODO")
(defvar alternative-font
  (font-spec :family "Source Code Pro" :size 14)
  "TODO")
(defvar hangul-font
  (font-spec :family "Noto Sans CJK KR" :size 14)
  "TODO")

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 1)
  (set-face-font 'default default-font)
  (set-fontset-font t '(#xE000 . #xF8FF) default-font) ; for ligature
  (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) default-font) ; for ligature
  (set-fontset-font "fontset-default" 'hangul hangul-font)
  (add-to-list 'face-font-rescale-alist '("DejaVu Sans" . 0.9)))

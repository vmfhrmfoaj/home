(defvar default-font
  (font-spec :family "Cascadia Code" :size 17)
  ;; (font-spec :family "Fantasque Sans Mono" :size 19)
  "TODO")
(defvar hangul-font
  (font-spec :family "Noto Sans CJK KR" :size 15)
  "TODO")

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 0)
  (set-face-font 'default default-font)
  (set-fontset-font t '(#xE000 . #xF8FF) default-font) ; for ligature
  (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) default-font) ; for ligature
  (set-fontset-font "fontset-default" 'hangul hangul-font)
  (add-to-list 'face-font-rescale-alist '("DejaVu Sans" . 0.85)))

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 0)
  (let ((font (font-spec :family "Cascadia Code" :size 13)))
    (set-face-font 'default font)
    (set-fontset-font t '(#xE000 . #xF8FF) font)                  ; for ligature
    (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) font)) ; for ligature
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Color Emoji" :size 12))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans" :size 13) nil t)
  (set-fontset-font "fontset-default" 'hangul  (font-spec :family "Noto Sans CJK KR" :size 13))
  (set-fontset-font "fontset-default" 'kana    (font-spec :family "Noto Sans CJK JP" :size 13))
  (let ((font (font-spec :family "Noto Sans CJK SC" :size 13)))
    (set-fontset-font "fontset-default" 'bopomofo font)
    (set-fontset-font "fontset-default" 'han      font))
  (add-to-list 'face-font-rescale-alist '("Noto Sans Mono" . 0.85)))

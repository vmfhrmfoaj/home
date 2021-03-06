;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'func))

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 1)
  (let* ((font-size 10.5)
         (font-name "Cascadia Code")
         (font (font-spec :family font-name :size font-size :weight 'semi-bold)))
    (set-face-font 'default font)
    (set-fontset-font nil '(#xE000 . #xF8FF) font)               ; for ligature
    (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) font) ; for ligature
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Color Emoji" :size font-size))
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans"      :size font-size) nil t)
    (set-fontset-font "fontset-default" 'hangul  (font-spec :family "Noto Sans CJK KR" :size font-size))
    (set-fontset-font "fontset-default" 'kana    (font-spec :family "Noto Sans CJK JP" :size font-size))
    (let ((font (font-spec :family "Noto Sans CJK SC" :size font-size)))
      (set-fontset-font "fontset-default" 'bopomofo font)
      (set-fontset-font "fontset-default" 'han      font))
    (cond
     ((string-equal "Cascadia Code" font-name)
      (add-to-list 'face-font-rescale-alist '("DejaVu Sans"         . 0.95))
      (add-to-list 'face-font-rescale-alist '("Fantasque Sans Mono" . 1.15))
      (add-to-list 'face-font-rescale-alist '("Fira Code"           . 0.95))
      (add-to-list 'face-font-rescale-alist '("Liberation Mono"     . 0.95)))
     ((string-equal "Fira Code" font-name)
      (add-to-list 'face-font-rescale-alist '("DejaVu Sans"         . 0.95))
      (add-to-list 'face-font-rescale-alist '("Fantasque Sans Mono" . 1.15))
      (add-to-list 'face-font-rescale-alist '("Liberation Mono"     . 0.95))
      (add-to-list 'face-font-rescale-alist '("Noto Sans"           . 0.95)))
     ((string-equal "Fantasque Sans Mono" font-name)
      (add-to-list 'face-font-rescale-alist '("Cascadia Code"       . 0.85))
      (add-to-list 'face-font-rescale-alist '("DejaVu Sans"         . 0.8))
      (add-to-list 'face-font-rescale-alist '("Fira Code"           . 0.8))
      (add-to-list 'face-font-rescale-alist '("Font Awesome 5 Free" . 0.9))
      (add-to-list 'face-font-rescale-alist '("Liberation Mono"     . 0.8))
      (add-to-list 'face-font-rescale-alist '("Noto Emoji"          . 0.8))
      (add-to-list 'face-font-rescale-alist '("Noto Color Emoji"    . 0.9))
      (add-to-list 'face-font-rescale-alist '("Noto Sans"           . 0.9))))))


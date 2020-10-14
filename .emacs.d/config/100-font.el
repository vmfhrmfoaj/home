;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(when window-system
  (prefer-coding-system 'utf-8)
  (setq-default line-spacing 0)
  (let* ((font-size 14)
         (font (font-spec :family "Cascadia Code" :size font-size :weight 'light)))
    (set-face-font 'default font)
    (set-fontset-font nil '(#xE000 . #xF8FF) font)               ; for ligature
    (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) font) ; for ligature
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Color Emoji" :size 11))
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans" :size font-size) nil t)
    (set-fontset-font "fontset-default" 'hangul  (font-spec :family "Noto Sans CJK KR" :size font-size))
    (set-fontset-font "fontset-default" 'kana    (font-spec :family "Noto Sans CJK JP" :size font-size))
    (let ((font (font-spec :family "Noto Sans CJK SC" :size font-size)))
      (set-fontset-font "fontset-default" 'bopomofo font)
      (set-fontset-font "fontset-default" 'han      font)))
  (add-to-list 'face-font-rescale-alist '("Fira Code" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Noto Sans Mono" . 0.9)))

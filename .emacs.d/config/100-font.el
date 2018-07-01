(when (window-system)
  (setq-default line-spacing 1)
  ;; In Linux the font hinting must not be 'full', it causes the bold font make smaller.
  ;; Options(only available on macOS):
  ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 0~3
  ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 0~16
  ;; Options(x11):
  ;; - .Xresource:
  ;;   Xft.embolden: true
  ;;   Emacs.fontBackend: xft
  (let ((font "Fira Code")
        (height (if (eq 'darwin system-type) 141 98)))
    (set-face-font 'default font)
    (set-face-attribute 'default nil
                        :weight 'medium
                        :width 'medium
                        :height height)
    (when (string-equal "Fira Code" font)
      (let ((alist (append
                    '(( 33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                      ( 35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                      ( 36 . ".\\(?:>\\)")
                      ( 37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                      ( 38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                      ( 42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*>]\\)") ; '*/' was deleted.
                      ( 43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                      ( 45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                      ( 46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                      ( 47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[/=]\\)") ; '/*' and '/>' were deleted.
                      ( 48 . ".\\(?:x[a-zA-Z]\\)")
                      ( 58 . ".\\(?:::\\|[:=]\\)")
                      ( 59 . ".\\(?:;;\\|;\\)")
                      ( 60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~<=>|-]\\)") ; '</' is deleted.
                      ( 61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                      ( 62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                      ( 91 . ".\\(?:]\\)")
                      ( 92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                      ( 94 . ".\\(?:=\\)")
                      (119 . ".\\(?:ww\\)")
                      (123 . ".\\(?:-\\)")
                      (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                      (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))
                    (when (functionp 'mac-auto-operator-composition-mode)
                      ;; NOTE
                      ;;  It causes freezing when opening the popup of `helm', except `emacs-mac'.
                      '((63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)"))))))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring]))))))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Free-Symbola" . 0.95))
  (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral" . 0.9))
  (add-to-list 'face-font-rescale-alist '("all-the-icons" . 0.9))
  (add-to-list 'face-font-rescale-alist '("file-icons" . 0.9))
  (add-to-list 'face-font-rescale-alist '("FontAwesome" . 0.9))
  (add-to-list 'face-font-rescale-alist '("github-octicons" . 0.85))
  ;; (add-to-list 'face-font-rescale-alist '("Weather Icons" . 0.9))
  ;; (add-to-list 'face-font-rescale-alist '("Material Icons" . 0.9))
  )

(prefer-coding-system 'utf-8)

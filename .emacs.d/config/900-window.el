(tool-bar-mode   -1)
(scroll-bar-mode -1)

(when window-system
  (let ((w 150))
    (setq org-tags-column (+ (- w) 5))
    (let* ((h (- (/ (display-pixel-height) (frame-char-height))
                 (if (zerop (% (display-pixel-height) (frame-char-height)))
                     2 1)
                 2))
           (l (/ (custom-display-pixel-width) 2.0))
           (l (floor (- l (* (frame-unit->pixel w) 0.45))))
           (l (if (< 0 (- (custom-display-pixel-width)
                          (+ l (frame-unit->pixel w))))
                  l
                (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
      (add-to-list 'default-frame-alist (cons 'width  w))
      (add-to-list 'default-frame-alist (cons 'height h))
      (setq split-width-threshold (1+ w)
            initial-frame-alist (list (cons 'top    (1+ (frame-char-height)))
                                      (cons 'left   l)
                                      (cons 'width  w)
                                      (cons 'height h))))))

(use-package winum
  :ensure t
  :init
  (defvar unicode-nums
    '("⓪" "①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨"
      "⑩" "⑪" "⑫" "⑬" "⑭" "⑮" "⑯" "⑰" "⑱" "⑲" "⑳"))

  (defun winum--num-str-to-pertty-num-str (num-str)
    "TODO"
    (let* ((active? (powerline-selected-window-active))
           (num (string-to-number num-str))
           (num-str (if (<= 0 num 20)
                        (nth num unicode-nums)
                      (number-to-string num)))
           (raise (if (<= 0 num 20) 0.05 0)))
      (propertize (concat "  " num-str)
                  'face (if active? 'powerline-active0 'powerline-inactive0)
                  'display `(raise ,raise))))

  (defun winum--assign-0-to-neotree ()
    "winum assign function for NeoTree."
    (when (string-match-p "\\*NeoTree\\*" (buffer-name)) 0))

  :config
  (add-to-list 'winum-assign-functions #'winum--assign-0-to-neotree)
  (advice-add #'winum-get-number-string :filter-return #'winum--num-str-to-pertty-num-str)
  (winum-mode))

(use-package zoom
  :ensure t
  :init
  (defun zoom--update-custom ()
    "Update the window layout in the current frame. (custom ver)"
    (let ((zoom-mode nil)
          (window-configuration-change-hook nil)
          (window-combination-resize t)
          (window-resize-pixelwise t))
      (unless (zoom--window-ignored-p)
        (balance-windows)
        (zoom--resize)
        (zoom--fix-scroll))))

  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
  (advice-add #'zoom--update :override #'zoom--update-custom)
  (zoom-mode t))

(tool-bar-mode   -1)
(scroll-bar-mode -1)
(when (or (eq 'gnu/linux system-type)
          (and (not window-system)
               (eq 'darwin system-type)))
  (menu-bar-mode -1))

(when window-system
  (let ((w 150)
        (h-char (frame-char-height))
        (h-titlebar-in-char 2)
        (top-offset (if (string-equal "gnome-imac" hostname) 5 0)))
    (setq org-tags-column (+ (- w) 5))
    (let* ((h (floor (- (/ (float (display-pixel-height)) h-char) h-titlebar-in-char)))
           (l (/ (custom-display-pixel-width) 2.0))
           (l (floor (- l (* (frame-unit->pixel w) 0.4))))
           (l (if (< 0 (- (custom-display-pixel-width)
                          (+ l (frame-unit->pixel w))))
                  l
                (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
      (add-to-list 'default-frame-alist (cons 'width  w))
      (add-to-list 'default-frame-alist (cons 'height h))
      (setq split-width-threshold (1+ w)
            initial-frame-alist (list (cons 'top    0)
                                      (cons 'left   l)
                                      (cons 'width  w)
                                      (cons 'height h))))))

(use-package winum
  :ensure t
  :init
  (defvar winum-num-strs
    '("(0)"  "(1)"  "(2)"  "(3)"  "(4)"  "(5)"  "(6)"  "(7)"  "(8)"  "(9)"
      "(10)" "(11)" "(12)" "(13)" "(14)" "(15)" "(16)" "(17)" "(18)" "(19)"))

  (defun winum--num-str-to-pertty-num-str (num-str)
    "TODO"
    (let* ((active? (powerline-selected-window-active))
           (num (string-to-number num-str))
           (num-str (if (<= 0 num 20)
                        (nth num winum-num-strs)
                      (number-to-string num))))
      (propertize (concat "  " num-str)
                  'face (if active?
                            'powerline-active0
                          'powerline-inactive0))))

  (defun winum--assign-0-to-neotree ()
    "winum assign function for NeoTree."
    (when (and (string-match-p "\\*NeoTree\\*" (buffer-name))
               (not (aref (winum--get-window-vector) 0)))
      0))

  :config
  (add-to-list 'winum-assign-functions #'winum--assign-0-to-neotree)
  (advice-add #'winum-get-number-string :filter-return #'winum--num-str-to-pertty-num-str)
  (winum-mode))

(use-package zoom
  :ensure t
  :init
  (defun zoom--handler-for-helm (&optional window-or-frame norecord)
    (unless (bound-and-true-p helm-alive-p)
      (zoom--handler window-or-frame norecord)))

  (defun zoom--on-for-helm ()
    "TODO"
    ;; register the zoom handler
    ;; NOTE
    ;;  It confilct `helm-display-buffer-at-bottom'
    (add-hook 'window-size-change-functions #'zoom--handler-for-helm)
    ;; (add-hook 'minibuffer-setup-hook #'zoom--handler)
    (advice-add #'select-window :after #'zoom--handler)
    ;; disable mouse resizing
    (advice-add #'mouse-drag-mode-line :override #'ignore)
    (advice-add #'mouse-drag-vertical-line :override #'ignore)
    (advice-add #'mouse-drag-header-line :override #'ignore)
    ;; update the layout once loaded
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (zoom--handler))))

  (defun zoom--update-for-helm ()
    "Update the window layout in the current frame. (custom ver)"
    (let ((zoom-mode nil)
          (window-configuration-change-hook nil)
          (window-combination-resize t)
          (window-resize-pixelwise t))
      ;; NOTE
      ;;  It confilct `helm-autoresize-mode'
      ;; (balance-windows)
      (unless (zoom--window-ignored-p)
        (balance-windows)
        (zoom--resize)
        (zoom--fix-scroll))))

  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-buffer-name-regexps '("\\*.*[Hh]elm.*\\*"))
  (advice-add #'zoom--on     :override #'zoom--on-for-helm)
  (advice-add #'zoom--update :override #'zoom--update-for-helm)
  (zoom-mode t))

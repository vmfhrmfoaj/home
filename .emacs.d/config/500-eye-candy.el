;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'auto-dim-other-buffers nil t)
  (require 'evil-ex nil t)
  (require 'composite nil t)
  (require 'diminish nil t)
  (require 'evil-goggles nil t)
  (require 'focus nil t)
  (require 'highlight-parentheses nil t)
  (require 'highlight-numbers nil t)
  (require 'hl-line nil t)
  (require 'hl-todo nil t)
  (require 'powerline nil t)
  (require 'spaceline-config nil t)
  (require 'vi-tilde-fringe nil t)
  (require 'yascroll nil t))

(use-package auto-dim-other-buffers
  :ensure t
  :init
  (defface auto-dim-other-line-number-face
    '((t (:inherit auto-dim-other-buffers-face)))
    "TODO")

  :config
  (defun adob--manually-dim (wnd)
    "Dim the background without conditions."
    (when (and (windowp wnd)
               (not (window-parameter wnd 'adob--dim)))
      (setq adob--last-window nil
            adob--last-buffer nil)
      (set-window-parameter wnd 'adob--dim t)
      (force-window-update wnd)))

  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (defvar-local adob--face-mode-remapping-for-line-number nil)

  (defconst adob--remap-face-for-line-number
    (if adob--adow-mode
        '(:filtered (:window adob--dim t) auto-dim-other-line-number-face)
      'auto-dim-other-buffers-face))

  (advice-add #'adob--kill-all-local-variables-advice :after
              (lambda (&rest _)
                "Customize `adob--kill-all-local-variables-advice' for `line-number'"
                (when adob--face-mode-remapping-for-line-number
                  (setq adob--face-mode-remapping-for-line-number
                        (face-remap-add-relative 'line-number adob--remap-face-for-line-number)))))

  (advice-add #'adob--remap-face :override
              (lambda (buffer object)
                "Customize `adob--remap-face' for `line-number'"
                (let ((wants (not (adob--never-dim-p buffer))))
                  (when (eq wants (not (buffer-local-value 'adob--face-mode-remapping-for-line-number buffer)))
                    (set-buffer buffer)
                    (setq adob--face-mode-remapping-for-line-number
                          (if wants
                              (face-remap-add-relative 'line-number adob--remap-face-for-line-number)
                            (face-remap-remove-relative adob--face-mode-remapping-for-line-number)
                            nil)))
                  (when (eq wants (not (buffer-local-value 'adob--face-mode-remapping buffer)))
                    (set-buffer buffer)
                    (setq adob--face-mode-remapping
                          (if wants
                              (face-remap-add-relative 'default adob--remap-face)
                            (face-remap-remove-relative adob--face-mode-remapping)
                            nil))
                    (force-window-update object)
                    wants))))

  (advice-add #'adob--rescan-windows :before-until
              (lambda ()
                "Prevent to dim buffers if minibuffer is focused."
                (minibuffer-window-active-p (minibuffer-window))))

  (auto-dim-other-buffers-mode 1))

(use-package composite
  :defer t
  :if (version<= "27.0" emacs-version)
  :init
  (defvar composition-ligature-table (make-char-table nil))

  (defvar ligature-regex
    ;; 1. https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-char-table
    ;; 2. https://github.com/belluzj/fantasque-sans/issues/64
    ;; 3. https://github.com/belluzj/fantasque-sans/pull/114
    '(( 33 . ".\\(?:==?\\)")                    ; !=, !==
      ( 35 . ".\\(?:_(\\|[{(?_]\\)")            ; #{}, #(), #_, #_(), #?
      ( 38 . ".\\(?:&\\)")                      ; &&
      ( 45 . ".\\(?:>>?\\)")                    ; ->, ->>
      ( 46 . ".\\(?:\\.\\.?\\|-\\)")            ; .., ..., .-
      ( 58 . ".\\(?::\\)")                      ; ::
      ( 60 . ".\\(?:=\\)")                      ; <=
      ( 61 . ".\\(?:==?\\)")                    ; ==, ===
      ( 62 . ".\\(?:=\\)")                      ; >=
      (124 . ".\\(?:|\\)")                      ; ||
      )
    "TODO")

  :hook
  (((text-mode prog-mode cider-repl-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))

  :config
  (-when-let (alist ligature-regex)
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (with-eval-after-load "cc-cmds"
    ;; NOTE
    ;;  When rendering ligature letters on mode-line, Emacs increases CPU utilization to 100%.
    (advice-add #'c-update-modeline :override (lambda ()))))

(use-package diminish
  :ensure t
  :config
  (with-eval-after-load "aggressive-indent"       (diminish 'aggressive-indent-mode      ""))
  (with-eval-after-load "auto-dim-other-buffers"  (diminish 'auto-dim-other-buffers-mode ""))
  (with-eval-after-load "abbrev"                  (diminish 'abbrev-mode                 ""))
  (with-eval-after-load "autorevert"              (diminish 'auto-revert-mode            ""))
  (with-eval-after-load "company"                 (diminish 'company-mode                ""))
  (with-eval-after-load "editorconfig"            (diminish 'editorconfig-mode           ""))
  (with-eval-after-load "eldoc"                   (diminish 'eldoc-mode                  ""))
  (with-eval-after-load "elisp-slime-nav"         (diminish 'elisp-slime-nav-mode        ""))
  (with-eval-after-load "evil-goggles"            (diminish 'evil-goggles-mode           ""))
  (with-eval-after-load "evil-org"                (diminish 'evil-org-mode               ""))
  (with-eval-after-load "flycheck"                (diminish 'flycheck-mode               ""))
  (with-eval-after-load "gcmh"                    (diminish 'gcmh-mode                   ""))
  (with-eval-after-load "git-gutter"              (diminish 'git-gutter-mode             ""))
  (with-eval-after-load "golden-ratio"            (diminish 'golden-ratio-mode           ""))
  (with-eval-after-load "highlight-parentheses"   (diminish 'highlight-parentheses-mode  ""))
  (with-eval-after-load "linum-relative"          (diminish 'linum-relative-mode         ""))
  (with-eval-after-load "ivy"                     (diminish 'ivy-mode                    ""))
  (with-eval-after-load "ivy-posframe"            (diminish 'ivy-posframe-mode           ""))
  (with-eval-after-load "magit-blame"             (diminish 'magit-blame-mode            ""))
  (with-eval-after-load "magit-svn"               (diminish 'magit-svn-mode              ""))
  (with-eval-after-load "org-indent"              (diminish 'org-indent-mode             ""))
  (with-eval-after-load "org-table"               (diminish 'orgtbl-mode                 ""))
  (with-eval-after-load "simple"                  (diminish 'auto-fill-function          ""))
  (with-eval-after-load "smartparens"             (diminish 'smartparens-mode            ""))
  (with-eval-after-load "subword"                 (diminish 'subword-mode                ""))
  (with-eval-after-load "undo-tree"               (diminish 'undo-tree-mode              ""))
  (with-eval-after-load "vi-tilde-fringe"         (diminish 'vi-tilde-fringe-mode        ""))
  (with-eval-after-load "view"                    (diminish 'view-mode                   ""))
  (with-eval-after-load "which-key"               (diminish 'which-key-mode              ""))
  (with-eval-after-load "yasnippet"               (diminish 'yas-minor-mode              ""))
  (with-eval-after-load "zoom"                    (diminish 'zoom-mode                   "")))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (setq evil-goggles-duration 0.08
        evil-goggles-pulse nil)

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq-local evil-goggles-duration 0.001)))

  (evil-goggles-mode))

(use-package focus
  :ensure t
  :defer t
  :commands (focus-init focus-terminate)
  :init
  (defvar focus--exclude-modes '(term-mode eshell-mode))

  (defvar-local focus--focus-move-timer nil)

  (defun focus-move-focus-with-timer ()
    (when (timerp focus--focus-move-timer)
      (cancel-timer focus--focus-move-timer))
    (setq focus--focus-move-timer
          (run-with-idle-timer 0.25 nil
                               (lambda (buf)
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (focus-move-focus))))
                               focus-buffer)))

  (defun focus--enable (&rest _)
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (minibufferp))
      (let ((cur-buf (current-buffer))
            (adob--adow-mode nil)
            (golden-ratio-mode nil))
        (--each (->> (window-list)
                     (--map (window-buffer it))
                     (-distinct))
          (when-let ((win (get-buffer-window it)))
            (with-selected-window win
              (with-current-buffer it
                (focus-init)
                (remove-hook 'post-command-hook 'focus-move-focus t)
                (if (equal it cur-buf)
                    (progn
                      (add-hook 'post-command-hook 'focus-move-focus-with-timer nil t)
                      (focus-move-focus))
                  (focus-move-overlays (point-min) (point-min))
                  (setq cursor-type 'hollow))))))
        (redisplay t))))

  (defun focus--disable (&rest _)
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (minibufferp))
      (let ((adob--adow-mode nil)
            (golden-ratio-mode nil))
        (--each (->> (window-list)
                     (--map (window-buffer it))
                     (-distinct))
          (when-let ((win (get-buffer-window it)))
            (with-selected-window win
              (with-current-buffer it
                (remove-hook 'post-command-hook 'focus-move-focus-with-timer t)
                (when (timerp focus--focus-move-timer)
                  (cancel-timer focus--focus-move-timer)
                  (setq focus--focus-move-timer nil))
                (setq cursor-type 'hollow)
                (focus-terminate)))))
        (evil-refresh-cursor)
        (redisplay t))))

  (defvar company--pseudo-tooltip-bg-lines nil)
  (defvar company--pseudo-tooltip-start-line nil)
  (defvar focus--beg-line nil)
  (defvar focus--end-line nil)
  (defun company--decorate-background-string (args)
    (if (and focus-pre-overlay
             focus-post-overlay)
        (-let* (((lines column-offset old column nl align-top) args)
                (old-len (length old)))
          (unless company--pseudo-tooltip-bg-lines
            (setq company--pseudo-tooltip-bg-lines
                  (--map (propertize it 'face 'focus-unfocused) old)))
          (if (null align-top)
              (let* ((num-out-of-scope-lines (- (+ company--pseudo-tooltip-start-line old-len) 1 focus--end-line)))
                (setq old (-concat (-drop-last num-out-of-scope-lines old)
                                   (-take-last num-out-of-scope-lines company--pseudo-tooltip-bg-lines))))
            (let* ((num-out-of-scope-lines (min old-len (- focus--beg-line company--pseudo-tooltip-start-line))))
              (setq old (-concat (-take num-out-of-scope-lines company--pseudo-tooltip-bg-lines)
                                 (-drop num-out-of-scope-lines old)))))
          (list lines column-offset old column nl align-top))
      args))

  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook  #'focus--disable)

  :config
  (defun focus--lisp-thing ()
    (or (save-excursion
          (ignore-errors
            (while (progn
                     (backward-up-list 1 t t)
                     (not (looking-at-p "(\\(defun\\|defmacro\\|lambda\\)\\_>"))))
            (let* ((end (save-excursion
                          (forward-list)
                          (point))))
              (cons (point) end))))
        (bounds-of-thing-at-point 'defun)))

  (defun focus--clojure-thing ()
    (or (save-excursion
          (ignore-errors
            (let ((pos nil))
              (while (progn
                       (backward-up-list 1 t t)
                       (not (or (and (looking-at "(")
                                     (char-equal ?# (char-before)))
                                (looking-at "(\\(?:[-0-9A-Za-z]+/\\)?\\(fn\\|def[a-z]*\\|reify\\|proxy\\|testing\\)\\_>"))))
                (setq pos (point)))
              (when (string-match-p "reify\\|proxy" (or (match-string-no-properties 1) ""))
                (if pos
                    (goto-char pos)
                  (throw 'error "No method"))))
            (let* ((end (save-excursion
                          (forward-list)
                          (point))))
              (cons (point) end))))
        (bounds-of-thing-at-point 'defun)))

  (defun focus--cider-repl-thing ()
    (or (when-let ((beg-mark cider-repl-input-start-mark))
          (save-excursion
            (goto-char beg-mark)
            (forward-sexp)
            (cons (marker-position beg-mark) (point))))
        (save-excursion
          (let ((beg (progn
                       (beginning-of-line)
                       (point))))
            (cons beg
                  (progn
                    (forward-sexp)
                    (point)))))))

  (defun focus--go-thing ()
    (when-let ((bound
                (or (save-excursion                   ; lambda
                      (ignore-errors
                        (while (progn
                                 (backward-up-list 1 t t)
                                 (backward-list 1)
                                 (not (looking-back "func\\s-*"))))
                        (skip-chars-forward "^{")
                        (let* ((end (save-excursion
                                      (forward-list)
                                      (point))))
                          (beginning-of-line-text)
                          (cons (point) end))))
                    (bounds-of-thing-at-point 'defun) ; func
                    (save-excursion                   ; struct, interface
                      (ignore-errors
                        (let ((regex "\\_<type\\s-+[0-9A-Za-z]+\\s-+\\(struct\\|interface\\)\\s-*"))
                          (while (progn
                                   (backward-up-list 1 t t)
                                   (not (looking-back regex (line-beginning-position)))))
                          (when (looking-back regex (line-beginning-position))
                            (let ((beg (match-beginning 0))
                                  (end (progn
                                         (forward-list)
                                         (point))))
                              (cons beg end))))))
                    (save-excursion                   ; const, import
                      (ignore-errors
                        (let ((regex "\\_<\\(const\\|import\\)\\s-*"))
                          (while (progn
                                   (backward-up-list 1 t t)
                                   (not (looking-back regex (line-beginning-position)))))
                          (when (looking-back regex (line-beginning-position))
                            (let ((beg (match-beginning 0))
                                  (end (progn
                                         (forward-list)
                                         (point))))
                              (cons beg end)))))))))
      (save-excursion
        (goto-char (car bound))
        (forward-comment -999)
        (cons (point) (cdr bound)))))

  (defun focus--python-thing ()
    (when-let ((bound (bounds-of-thing-at-point 'defun)))
      (save-excursion
        (goto-char (cdr bound))
        (skip-chars-forward " \t\r\n")
        (cons (car bound) (point)))))

  (defun focus--rpm-spec-thing ()
    (let ((beg (progn
                 (or (and (save-excursion (re-search-backward "^%[a-z]+$" nil t))
                          (match-beginning 0))
                     (point-min))))
          (end (progn
                 (or (and (save-excursion (re-search-forward  "^%[a-z]+$" nil t))
                          (match-beginning 0))
                     (point-max)))))
      (cons beg end)))

  (defun focus--org-thing ()
    (save-excursion
      (let ((beg (progn
                   (outline-previous-heading)
                   (point)))
            (end   (progn
                     (outline-next-visible-heading 1)
                     (line-beginning-position))))
        (cons beg end))))

  (with-eval-after-load "clojure-mode"
    (put 'clojure    'bounds-of-thing-at-point #'focus--clojure-thing)
    (put 'cider-repl 'bounds-of-thing-at-point #'focus--cider-repl-thing)
    (add-to-list 'focus-mode-to-thing '(clojure-mode    . clojure))
    (add-to-list 'focus-mode-to-thing '(cider-repl-mode . cider-repl)))
  (with-eval-after-load "elisp-mode"
    (put 'lisp 'bounds-of-thing-at-point #'focus--lisp-thing)
    (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp)))
  (with-eval-after-load "go-mode"
    (put 'go 'bounds-of-thing-at-point #'focus--go-thing)
    (add-to-list 'focus-mode-to-thing '(go-mode . go)))
  (with-eval-after-load "python-mode"
    (put 'py 'bounds-of-thing-at-point #'focus--python-thing)
    (add-to-list 'focus-mode-to-thing '(python-mode . py)))
  (with-eval-after-load "rpm-spec-mode"
    (put 'rpm-spec 'bounds-of-thing-at-point #'focus--rpm-spec-thing)
    (add-to-list 'focus-mode-to-thing '(rpm-spec-mode . rpm-spec)))
  (with-eval-after-load "org"
    (put 'org 'bounds-of-thing-at-point #'focus--org-thing)
    (add-to-list 'focus-mode-to-thing '(org-mode . org)))

  (with-eval-after-load "company"
    (advice-add #'company-pseudo-tooltip-show :before
                (lambda (&rest _)
                  "Init variables for `company--decorate-background-string' function"
                  (setq company--pseudo-tooltip-bg-lines nil
                        company--pseudo-tooltip-start-line (1+ (line-number-at-pos company-point))
                        focus--beg-line (-some-> focus-pre-overlay  (overlay-end)   (line-number-at-pos))
                        focus--end-line (-some-> focus-post-overlay (overlay-start) (line-number-at-pos)))))
    (advice-add #'company--replacement-string :filter-args #'company--decorate-background-string)))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (with-eval-after-load "company"
    (add-hook 'company-completion-started-hook
              (lambda (&rest _)
                (when (derived-mode-p 'prog-mode)
                  (highlight-parentheses-mode -1))))
    (add-hook 'company-after-completion-hook
              (lambda (&rest _)
                (when (derived-mode-p 'prog-mode)
                  (highlight-parentheses-mode  1)))))

  (defvar highlight-parentheses--timer nil)

  (advice-add #'highlight-parentheses--initiate-highlight :around
              (lambda (fn &rest args)
                "To avoid frequently run in the short time."
                (when highlight-parentheses--timer
                  (cancel-timer highlight-parentheses--timer))
                (setq highlight-parentheses--timer
                      (run-at-time 0.15 nil
                                   (lambda ()
                                     (setq highlight-parentheses--timer nil)
                                     (apply fn args)))))))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook ((latex-mode prog-mode rpm-spec-mode toml-mode) . highlight-numbers-mode)
  :config
  (defconst highlight-numbers-generic-regexp
    (rx (and
         symbol-start
         (? (or "+" "-"))
         (+ (or digit "." "_"))
         (? (or "e" "E") (? (or "+" "-")) (+ (or digit "_")))
         symbol-end))
    "Customize `highlight-numbers-generic-regexp' to highlight the negative number."))

(use-package hl-line
  :disabled t
  :config
  (global-hl-line-mode))

(use-package hl-todo
  :ensure t
  :hook ((latex-mode prog-mode) . hl-todo-mode)
  :config
  (defun hl-todo--setup-custom ()
    "TODO"
    (font-lock-remove-keywords nil hl-todo--keywords)
    (setq hl-todo--keywords
          (list (list (caar hl-todo--keywords)
                      `(1 (hl-todo--get-face) prepend))))
    (font-lock-add-keywords nil hl-todo--keywords t))

  (advice-add #'hl-todo--get-face :filter-return #'list)
  (advice-add #'hl-todo--setup :after #'hl-todo--setup-custom))

(use-package ivy-posframe
  :ensure t
  :init
  (eval-when-compile (require 'ivy-posframe nil t))

  :config
  (defun ivy-posframe--re-display (str &optional poshandler)
    "Improve the performance."
    (let ((buf (get-buffer ivy-posframe-buffer)))
      (when (-some-> buf
                     (with-current-buffer posframe--frame)
                     (frame-visible-p))
        (with-current-buffer buf
          (posframe--insert-string str nil)
          (setq-local truncate-lines ivy-truncate-lines))
        (with-ivy-window
          (ivy-posframe--add-prompt 'ignore))
        t)))

  (defun posframe-poshandler-point-bottom (info &optional font-height upward)
    (let* ((y-pixel-offset (plist-get info :y-pixel-offset))
           (posframe-width (plist-get info :posframe-width))
           (posframe-height (plist-get info :posframe-height))
           (window (plist-get info :parent-window))
           (xmax (plist-get info :parent-frame-width))
           (ymax (plist-get info :parent-frame-height))
           (position-info
            (or
             ;; :position-info has been removed, this line
             ;; is used for compatible.
             (plist-get info :position-info)
             (plist-get info :position)))
           (position-info
            (if (integerp position-info)
                (posn-at-point position-info window)
              position-info))
           (header-line-height (plist-get info :header-line-height))
           (tab-line-height (plist-get info :tab-line-height))
           (x (car (window-inside-pixel-edges window)))
           (y-top (+ (cadr (window-pixel-edges window))
                     tab-line-height
                     header-line-height
                     (- (or (cdr (posn-x-y position-info)) 0)
                        ;; Fix the conflict with flycheck
                        ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                        (or (cdr (posn-object-x-y position-info)) 0))
                     y-pixel-offset))
           (font-height (or font-height (plist-get info :font-height)))
           (y-bottom (+ y-top font-height)))
      (cons (max 0 (min x (- xmax (or posframe-width 0))))
            (max 0 (if (if upward
                           (> (- y-bottom (or posframe-height 0)) 0)
                         (> (+ y-bottom (or posframe-height 0)) ymax))
                       (- y-top (or posframe-height 0))
                     y-bottom)))))

  (defun ivy-posframe-custom-display-at-point (str)
    (ivy-posframe--display str #'posframe-poshandler-point-bottom))

  (defun ivy-posframe--custom-add-prompt (fn &rest args)
    "Override to mimic Evil style cursor."
    (apply fn args)
    (when (and (display-graphic-p)
               (not ivy-posframe--ignore-prompt))
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (let ((point (point))
              (prompt (buffer-string))
              (state evil-state))
          (remove-text-properties 0 (length prompt) '(read-only nil) prompt)
          (with-current-buffer ivy-posframe-buffer
            (goto-char (point-min))
            (delete-region (point) (line-beginning-position 2))
            (insert prompt "  \n")
            (add-text-properties
             point (1+ point)
             (if (eq 'insert state)
                 ;; NOTE
                 ;;  See, https://stackoverflow.com/a/23813217
                 `(display ,(compose-chars (char-after point) '(base-right  . base-right)  ?▏))
               '(face ivy-posframe-cursor))))))))

  (setq ivy-posframe-display-functions-alist
        '((counsel-company . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-frame-center)))

  (with-eval-after-load "swiper"
    (-update->> ivy-update-fns-alist
                (--remove (-let (((caller . _rest) it))
                            (eq 'swiper caller)))))

  (advice-add #'ivy-posframe--display :before-until #'ivy-posframe--re-display)
  (advice-add #'ivy-posframe-display-at-point :override #'ivy-posframe-custom-display-at-point)
  (advice-add #'ivy-posframe--add-prompt :override #'ivy-posframe--custom-add-prompt)
  (advice-add #'ivy-keyboard-quit :before
              (lambda ()
                "Kill `ivy-posframe-buffer'."
                (posframe-delete ivy-posframe-buffer)))

  (advice-add #'ivy-posframe-display-at-frame-center :after
              (lambda (str)
                "Customize for `auto-dim-other-buffers-mode', `fringe-mode' and `line-number-mode'"
                (when-let ((wnd (ivy-state-window ivy-last)))
                  (when-let ((buf (ivy-state-buffer ivy-last)))
                    (with-current-buffer buf
                      (setq powerline-selected-window nil)
                      (force-mode-line-update)))
                  (when (fboundp #'adob--manually-dim)
                    (adob--manually-dim wnd)))))

  (ivy-posframe-mode 1))

(use-package posframe
  :defer t
  :init
  (eval-when-compile (require 'posframe nil t))

  :config
  (defun posframe-mouse-avoidance (frame)
    (-let* (((mp-frame) (mouse-position)))
      (when (eq frame mp-frame)
        (set-mouse-position frame (frame-width frame) (frame-height frame))))
    frame)

  (setq posframe-mouse-banish nil)

  (advice-add #'posframe-show :filter-return #'posframe-mouse-avoidance))

(use-package powerline
  :ensure t)

(use-package spaceline-config
  :ensure spaceline
  :init
  (defface spaceline-symbol-segment-face
    '((t (:inherit shadow)))
    "TODO")

  (defcustom spaceline-symbol-segment--max-symbol-length 100
    "Max length of `spaceline-symbol-segment--symbol'.")

  (defvar-local spaceline-symbol-segment--symbol nil)

  :config
  (spaceline-define-segment symbol
    "Display the symbol"
    (when (and (stringp spaceline-symbol-segment--symbol)
               (< 0 (length spaceline-symbol-segment--symbol)))
      (let ((max-len spaceline-symbol-segment--max-symbol-length)
            (len (length spaceline-symbol-segment--symbol)))
        (propertize
         (if (<= len max-len)
             spaceline-symbol-segment--symbol
           (concat (propertize "..." 'face 'shadow)
                   (substring spaceline-symbol-segment--symbol (- len max-len) len)))
         'face 'spaceline-symbol-segment-face))))

  (defun spaceline--my-theme ()
    (spaceline-compile
      '((window-number
         :face highlight-face
         :priority 100)
        (anzu :priority 95)
        auto-compile
        ((buffer-id
          remote-host
          buffer-modified)
         :priority 98)
        (symbol :when active :priority 105))
      '((purpose :priority 94)
        (selection-info :priority 95)
        input-method
        (global :when active)
        ((flycheck-error flycheck-warning flycheck-info)
         :when active
         :priority 89)
        (erc-track :when active)
        (org-clock :when active)
        (process :when active)
        (minor-modes :when active :priority 9)
        (major-mode :when active :priority 79)))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  (setq spaceline-window-numbers-unicode t)

  (add-hook 'window-setup-hook
            (lambda ()
              (let ((fmt (spaceline--my-theme)))
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (setq mode-line-format fmt)))
                (force-mode-line-update t)))))

(use-package vi-tilde-fringe
  :disabled t
  :if (fboundp 'define-fringe-bitmap)
  :ensure t
  :config
  (global-vi-tilde-fringe-mode))

(use-package yascroll
  :disabled t
  :ensure t
  :defer t
  :init
  ;; NOTE
  ;;  `yascroll:show-scroll-bar' is very slow.
  ;;  I will manually trigger this function.
  (let ((fn (lambda (&rest _)
              "Display the scroll bar"
              (yascroll:show-scroll-bar))))
    (advice-add #'evil-scroll-line-to-top    :after fn)
    (advice-add #'evil-scroll-line-to-center :after fn)
    (advice-add #'evil-scroll-line-to-bottom :after fn)
    (advice-add #'recenter-top-bottom        :after fn)))

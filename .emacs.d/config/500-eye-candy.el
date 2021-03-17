;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (add-to-list 'auto-dim-other-buffers-never-dim-buffer-functions
               (lambda (buf)
                 "Disable dimming focused buffer while executing `evil-ex' function."
                 (and (eq buf (or
                               ;; NOTE:
                               ;;  When `adob--last-buffer' was set the minibuffer,
                               ;;   I guess latest buffer.
                               (and (minibufferp adob--last-buffer)
                                    (->> (buffer-list)
                                      (-drop-while #'minibufferp)
                                      (sort-buffer-by-visit-time)
                                      (car)))
                               adob--last-buffer))
                      (buffer-live-p evil-ex-current-buffer))))

  (with-eval-after-load "ivy-posframe"
    (add-to-list 'auto-dim-other-buffers-never-dim-buffer-functions
                 (lambda (buf)
                   "Disalbe dimming `ivy-posframe-buffer'."
                   (string-equal ivy-posframe-buffer (buffer-name buf)))))

  (setq auto-dim-other-buffers-dim-on-focus-out nil)

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
    '(( 33 . ".\\(?:==\\|[!=]\\)")              ; !=, !==
      ( 38 . ".\\(?:&\\)")                      ; &&
      ( 45 . ".\\(?:-?>>?\\)")                  ; ->, ->>, -->
      ( 46 . ".\\(?:\\.\\.?\\|-\\)")            ; .., ..., .-
      ( 58 . ".\\(?:=\\|::?\\)")                ; ::, :::, :=
      ( 60 . ".\\(?:=\\|\\(?:!-\\)?-\\)")       ; <=, <!--, <-
      ( 61 . ".\\(?:==?\\|>\\)")                ; ==, ===, =>
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
  (setq evil-goggles-duration 0.15)

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

  (defvar focus--focus-move-timer nil)

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
      (focus-init)
      (remove-hook 'post-command-hook 'focus-move-focus t)
      (add-hook    'post-command-hook 'focus-move-focus-with-timer nil t)
      (focus-move-focus)
      (redisplay t)))

  (defun focus--disable (&rest _)
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (minibufferp))
      (remove-hook 'post-command-hook 'focus-move-focus-with-timer t)
      (when (timerp focus--focus-move-timer)
        (cancel-timer focus--focus-move-timer)
        (setq focus--focus-move-timer nil))
      (focus-terminate)
      (redisplay t)))

  (defvar company--pseudo-tooltip-bg-lines nil)
  (defvar company--pseudo-tooltip-start-line nil)
  (defvar focus--beg-line nil)
  (defvar focus--end-line nil)
  (defun company--decorate-background-string (args)
    (if (and focus-pre-overlay
             focus-post-overlay)
        (-let* (((lines old column nl align-top) args)
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
          (list lines old column nl align-top))
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
            (while (progn
                     (backward-up-list 1 t t)
                     (not (looking-at-p "(\\([-0-9A-Za-z]+/\\)?\\(fn\\|def[a-z]*\\)\\_>"))))
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
    (ignore-errors
      (if focus-mode-org-thing-lock
          (cons 0 0)
        (save-excursion
          (let ((beg (progn
                       (outline-previous-heading)
                       (point)))
                (end   (progn
                         (outline-next-visible-heading 1)
                         (beginning-of-line)
                         (point))))
            (cons beg end))))))

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
  (with-eval-after-load "org-mode"
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
                  (highlight-parentheses-mode  1))))))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook ((prog-mode rpm-spec-mode toml-mode) . highlight-numbers-mode))

(use-package hl-line
  :disabled t
  :config
  (global-hl-line-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
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
  :config
  (setq ivy-posframe-display-functions-alist
        '((counsel-company . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))

  (with-eval-after-load "swiper"
    (-update->> ivy-update-fns-alist
      (--remove (-let (((caller . _rest) it))
                  (eq 'swiper caller)))))

  (with-eval-after-load "golden-ratio"
    (add-hook 'window-setup-hook
              (lambda ()
                (let ((w (-second-item (golden-ratio--dimensions))))
                  (setq ivy-posframe-width w
                        ivy-posframe-min-width w)))))

  (ivy-posframe-mode 1))

(use-package posframe
  :defer t
  :config
  (defun posframe-mouse-avoidance (frame)
    (-let* (((mp-frame) (mouse-position)))
      (when (eq frame mp-frame)
        (set-mouse-position frame (frame-width frame) 0)))
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

  (require 'treemacs-icons)
  (setq-default treemacs-icons
                (if (treemacs--should-use-tui-icons?)
                    (treemacs-theme->tui-icons treemacs--current-theme)
                  (treemacs-theme->gui-icons treemacs--current-theme)))

  (defvar spaceline-symbol-segment--major-icon-cache (make-hash-table))

  (spaceline-define-segment major-icon
    "Display a icon for `major-mode'"
    (if-let ((icon (gethash major-mode spaceline-symbol-segment--major-icon-cache)))
        icon
      (puthash major-mode
               (when-let ((disp (-some->> buffer-file-name
                                  (treemacs-icon-for-file)
                                  (get-text-property 0 'display))))
                 (when (listp disp)
                   (let ((icon (propertize "  " 'display
                                           (cl-list* 'image
                                                     (let ((h (frame-char-height)))
                                                       (-> disp
                                                         (cl-rest)
                                                         (cl-copy-list)
                                                         (plist-put :background (bg-color-from 'powerline-active1))
                                                         (plist-put :height h)
                                                         (plist-put :width h)))))))
                     (puthash major-mode icon spaceline-symbol-segment--major-icon-cache)
                     icon)))
               spaceline-symbol-segment--major-icon-cache)))

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
        (symbol :priority 105 :when active))
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
        (major-icon :fallback major-mode :priority 79)))
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
    (advice-add #'evil-scroll-line-to-bottom :after fn)))

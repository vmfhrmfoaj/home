;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package composite
  :defer t
  :if (version<= "27.0" emacs-version)
  :init
  (defvar composition-ligature-table (make-char-table nil))

  (defvar ligature-regex
    ;; 1. https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-char-table
    ;; 2. https://github.com/belluzj/fantasque-sans/issues/64
    ;; 3. https://github.com/belluzj/fantasque-sans/pull/114
    '(( 33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")               ; !=, !==
      ( 42 . ".\\(?:[*/]\\)")                            ; */
      ( 45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>-]\\)") ; -<, -<<, ->, ->>, -->
      ( 47 . ".\\(?:\\(?:\\*\\*/\\)\\|\\*\\|//?\\)")     ; /**/, /*, //, ///
      ( 58 . ".\\(?:\\(::\\|[:=]\\)\\)")                 ; ::, :::, :=
      ( 60 . ".\\(?:\\(?:|\\{2,3\\}\\|!--\\|~[~>]\\|-[<->]\\|<[<=-]\\|=[<=>]\\||>\\)\\|[~<=>|-]\\)") ; <<<, <<, <=, <|>, <>, <|||, <||, <|, <!--, <--, <<-, <-, <~~, <~, <<=, <==, <=<, <=>, <->, <~>
      ( 61 . ".\\(?:\\(?:<<\\|=[=>]\\|>>\\)\\|[<=>]\\)") ; ==, ===, =>, =>>, ==>, =<<
      ( 62 . ".\\(?:\\(?:>[=>-]\\)\\|[=>-]\\)")          ; >=, >>, >>>, >-, >>-, >>=
      (124 . ".\\(?:\\(?:|>\\|||>\\)\\|[>|]\\)")         ; |>, ||, ||>, |||>
      (126 . ".\\(?:\\(?:~>\\)\\|[>~]\\)")               ; ~>, ~~>
      ) "TODO")

  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
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
  (with-eval-after-load "auto-dim-other-buffers"  (diminish 'auto-dim-other-buffers-mode ""))
  (with-eval-after-load "abbrev"                  (diminish 'abbrev-mode                 ""))
  (with-eval-after-load "autorevert"              (diminish 'auto-revert-mode            ""))
  (with-eval-after-load "company"                 (diminish 'company-mode                ""))
  (with-eval-after-load "editorconfig"            (diminish 'editorconfig-mode           ""))
  (with-eval-after-load "eldoc"                   (diminish 'eldoc-mode                  ""))
  (with-eval-after-load "elisp-slime-nav"         (diminish 'elisp-slime-nav-mode        ""))
  (with-eval-after-load "evil-goggles"            (diminish 'evil-goggles-mode           ""))
  (with-eval-after-load "flycheck"                (diminish 'flycheck-mode               ""))
  (with-eval-after-load "git-gutter+"             (diminish 'git-gutter+-mode            ""))
  (with-eval-after-load "helm"                    (diminish 'helm-mode                   ""))
  (with-eval-after-load "helm-files"              (diminish 'helm-ff-cache-mode          ""))
  (with-eval-after-load "highlight-parentheses"   (diminish 'highlight-parentheses-mode  ""))
  (with-eval-after-load "linum-relative"          (diminish 'linum-relative-mode         ""))
  (with-eval-after-load "magit-blame"             (diminish 'magit-blame-mode            ""))
  (with-eval-after-load "magit-svn"               (diminish 'magit-svn-mode              ""))
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
  (defvar focus--exclude-modes '(term-mode))

  (defvar-local focus-face-remap-cookie nil)

  (defun focus--enable ()
    "TODO"
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (bound-and-true-p helm-alive-p)
                (minibufferp))
      (focus-init)
      (remove-hook 'post-command-hook 'focus-move-focus t)
      (focus-move-focus)
      (setq focus-face-remap-cookie
            (face-remap-add-relative 'hl-line-evil-insert 'bold))
      (redisplay t)))

  (defun focus--disable ()
    "TODO"
    (when focus-face-remap-cookie
      (face-remap-remove-relative focus-face-remap-cookie)
      (setq focus-face-remap-cookie nil))
    (focus-terminate)
    (redisplay t))

  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook  #'focus--disable)

  :config
  (defun focus--tex-thing ()
    "TODO"
    (ignore-errors
      (let* ((regx  (concat "^\\(?:[[:cntrl:]]\\)*$"))
             (beg (save-excursion
                    (backward-char)
                    (re-search-backward regx nil t)
                    (point)))
             (end   (save-excursion
                      (forward-char)
                      (re-search-forward regx nil t)
                      (point))))
        (cons beg end))))

  (defun focus--text-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (backward-paragraph)
                     (point)))
              (end (progn
                     (forward-paragraph)
                     (point))))
          (cons beg end)))))

  (defun focus--list+-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (cond ((sp-point-in-string)
                              (save-match-data
                                (re-search-backward "[^\\]\""))
                              (forward-char))
                             ((sp-point-in-comment)
                              (beginning-of-line)))
                       (backward-up-list 2))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (defun focus--lisp-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (while (progn
                                (backward-up-list 1 t t)
                                (not (looking-at-p "(\\(\\(lexical-\\)?let\\*?\\|lambda\\|defun\\|defmacro\\)\\_>")))))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (defun focus--clojure-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (while (progn
                                (backward-up-list 1 t t)
                                (not (looking-at-p "(\\([-0-9A-Za-z]+/\\)?\\(let\\|loop\\|doseq\\|fn\\|def[a-z]*\\)\\_>")))))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (with-eval-after-load "clojure-mode"
    (put 'clojure 'bounds-of-thing-at-point #'focus--clojure-thing)
    (put 'list+   'bounds-of-thing-at-point #'focus--list+-thing)
    (add-to-list 'focus-mode-to-thing '(clojure-mode    . clojure))
    (add-to-list 'focus-mode-to-thing '(cider-repl-mode . list+)))

  (let ((form
         '(progn
            (unless (fboundp 'focus--c-style-thing)
              (defun focus--c-style-thing ()
                "TODO"
                (ignore-errors
                  (save-excursion
                    (let (end)
                      (while (progn
                               (backward-up-list 1 t t)
                               (not (char-equal ?{ (char-after)))))
                      (save-excursion
                        (forward-list)
                        (setq end (point)))
                      (beginning-of-line)
                      (unless (looking-at-p "\\s-*}")
                        (let ((keep-going t)
                              (need-forward-line t))
                          (while (and (not (bobp)) keep-going)
                            (forward-line -1)
                            (beginning-of-line)
                            (when (or (looking-at-p "[[:space:]]*$")
                                      (looking-at-p "\\s-*/[*/]")
                                      (looking-at-p ".*,\\s-*\\(/[*/].*\\)?$")
                                      (looking-at-p ".*;\\s-*\\(/[*/].*\\)?$")
                                      (looking-at-p ".*{\\s-*\\(/[*/].*\\)?$"))
                              (when need-forward-line
                                (forward-line 1)
                                (beginning-of-line))
                              (setq keep-going nil)))))
                      (skip-chars-forward " \t}" )
                      (cons (point) end)))))
              (put 'c-style 'bounds-of-thing-at-point #'focus--c-style-thing)))))
    (eval-after-load "cc-mode"
      (append form '((add-to-list 'focus-mode-to-thing '(c-mode    . c-style))
                     (add-to-list 'focus-mode-to-thing '(c++-mode  . c-style))
                     (add-to-list 'focus-mode-to-thing '(java-mode . c-style)))))
    (eval-after-load "rust-mode"
      (append form '((add-to-list 'focus-mode-to-thing '(rust-mode . c-style))))))

  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp))
  (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
  (add-to-list 'focus-mode-to-thing '(text-mode . sentence+))
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--tex-thing)
  (put 'sentence+    'bounds-of-thing-at-point #'focus--text-thing)
  (put 'lisp         'bounds-of-thing-at-point #'focus--lisp-thing))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook ((prog-mode rpm-spec-mode) . highlight-numbers-mode))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)

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

(use-package powerline
  :ensure t
  :config
  (when window-system
    (setq powerline-height (+ (frame-char-height) line-spacing)
          powerline-text-scale-factor 1)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq spaceline-window-numbers-unicode t)

  (make-thread
   (lambda ()
     (let ((fmt (spaceline--theme
                 '((((window-number
                      projectile-root) :separator "|")
                    buffer-modified)
                   :face highlight-face
                   :priority 100)
                 '((buffer-id remote-host)
                   :priority 98))))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (setq mode-line-format fmt)))
       (spaceline-toggle-hud-off)
       (spaceline-toggle-version-control-off)
       (force-mode-line-update t)
       (spaceline-helm-mode)))))

(use-package vi-tilde-fringe
  :if (fboundp 'define-fringe-bitmap)
  :ensure t
  :config
  (global-vi-tilde-fringe-mode))

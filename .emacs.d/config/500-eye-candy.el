(use-package composite
  :defer t
  :if (version<= "27.0" emacs-version)
  :init
  (defvar composition-ligature-table (make-char-table nil))

  (defvar fantasque-sans-mono-ligature-regex
    ;; 1. https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-char-table
    ;; 2. https://github.com/belluzj/fantasque-sans/issues/64
    ;; 3. https://github.com/belluzj/fantasque-sans/pull/114
    '(( 33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")               ; !=, !==
      ( 42 . ".\\(?:[*/]\\)")                            ; */
      ( 45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>-]\\)") ; -<, -<<, ->, ->>, -->
      ( 47 . ".\\(?:\\(?:**/\\)\\|[/*]\\)")              ; /**/, /*, //
      ( 60 . ".\\(?:\\(?:|\\{2,3\\}\\|!--\\|~[~>]\\|-[<->]\\|<[<=-]\\|=[<=>]\\||>\\)\\|[~<=>|-]\\)") ; <<<, <<, <=, <|>, <>, <|||, <||, <|, <!--, <--, <<-, <-, <~~, <~, <<=, <==, <=<, <=>, <->, <~>
      ( 61 . ".\\(?:\\(?:<<\\|=[=>]\\|>>\\)\\|[<=>]\\)") ; ==, ===, =>, =>>, ==>, =<<
      ( 62 . ".\\(?:\\(?:>[=>-]\\)\\|[=>-]\\)")          ; >=, >>, >>>, >-, >>-, >>=
      (124 . ".\\(?:\\(?:|>\\|||>\\)\\|[>|]\\)")         ; |>, ||, ||>, |||>
      (126 . ".\\(?:\\(?:~>\\)\\|[>~]\\)")               ; ~>, ~~>
      ) "TODO")

  (defvar cascadia-code-ligature-regex
   '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
     (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
     (36 . ".\\(?:\\(>\\)>?\\)")
     (37 . ".\\(?:\\(%\\)%?\\)")
     (38 . ".\\(?:\\(&\\)&?\\)")
     (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
     (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
     (43 . ".\\(?:\\([>]\\)>?\\)")
     (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
     (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
     (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
     (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
     (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
     (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
     (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
     (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
     (59 . ".\\(?:\\(;\\);?\\)")
     (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
     (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
     (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
     (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
     (91 . ".\\(?:\\(|\\)[]|]?\\)")
     (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
     (94 . ".\\(?:\\(=\\)=?\\)")
     (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
     (119 . ".\\(?:\\(ww\\)w?\\)")
     (123 . ".\\(?:\\(|\\)[|}]?\\)")
     (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
     (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))
   "TODO")

  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))

  :config
  (-when-let (alist (cond
                     ((eq (intern "Cascadia Code")       (font-get default-font :family))
                      cascadia-code-ligature-regex)
                     ((eq (intern "Fantasque Sans Mono") (font-get default-font :family))
                      fantasque-sans-mono-ligature-regex)))
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
  (defvar focus--exclude-modes '(term-mode)
    "TODO")

  (defvar-local focus-face-remap-cookie nil)

  (defn focus--enable ()
    "TODO"
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (bound-and-true-p helm-alive-p)
                (minibufferp))
      (focus-init)
      (let ((focus-update-idle-time nil))
        (focus-move-focus))
      (setq focus-face-remap-cookie
            (face-remap-add-relative 'hl-line 'bold))))

  (defn focus--disable ()
    "TODO"
    (when focus-face-remap-cookie
      (face-remap-remove-relative focus-face-remap-cookie)
      (setq focus-face-remap-cookie nil))
    (focus-terminate))

  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook  #'focus--disable)

  :config
  (defn focus--tex-thing ()
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

  (defn focus--text-thing ()
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

  (defn focus--list+-thing ()
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

  (defn focus--lisp-thing ()
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

  (with-eval-after-load "clojure-mode"
    (defn focus--clojure-thing ()
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

    (put 'clojure 'bounds-of-thing-at-point #'focus--clojure-thing)
    (put 'list+   'bounds-of-thing-at-point #'focus--list+-thing)
    (add-to-list 'focus-mode-to-thing '(clojure-mode    . clojure))
    (add-to-list 'focus-mode-to-thing '(cider-repl-mode . list+)))

  (let ((form
         '(progn
            (unless (fboundp 'focus--c-style-thing)
              (defn focus--c-style-thing ()
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

  (defvar-local focus--update-timer nil
    "TODO")

  (defn focus--clear-update-timer ()
    "Clear `focus--update-timer`."
    (when focus--update-timer
      (cancel-timer focus--update-timer)
      (setq focus--update-timer nil)))

  (defn focus--wrap-move-focus (fn)
    "Delay to update `focus-pre-overlay' and `focus-post-overlay'."
    (when focus--update-timer
      (cancel-timer focus--update-timer))
    (if (not focus-update-idle-time)
        (funcall fn)
      (setq focus--update-timer
            (run-with-idle-timer
             focus-update-idle-time
             nil
             (lambda (buf fn)
               (with-current-buffer buf
                 (setq focus--update-timer nil)
                 (condition-case nil
                     (funcall fn)
                   (error (progn
                            (focus-terminate)
                            (focus-init)
                            (funcall fn))))))
             (current-buffer)
             fn))))

  (setq focus-update-idle-time 0.2)
  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp))
  (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
  (add-to-list 'focus-mode-to-thing '(text-mode . sentence+))
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--tex-thing)
  (put 'sentence+    'bounds-of-thing-at-point #'focus--text-thing)
  (put 'lisp         'bounds-of-thing-at-point #'focus--lisp-thing)

  (advice-add #'focus-terminate :after #'focus--clear-update-timer)
  (advice-add #'focus-move-focus :around #'focus--wrap-move-focus))

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
  (defn hl-todo--setup-custom ()
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
       (force-mode-line-update t)
       (spaceline-helm-mode)))))

(use-package vi-tilde-fringe
  :if (fboundp 'define-fringe-bitmap)
  :ensure t
  :config
  (global-vi-tilde-fringe-mode))

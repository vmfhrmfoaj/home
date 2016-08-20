;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     clojure
     elixir
     emacs-lisp
     erlang
     (focus :variables
            focus-mode-to-new-thing '((clojure-mode    . list+)
                                      (emacs-lisp-mode . list+)))
     git
     html
     java
     javascript
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill nil)
     markdown
     org
     sql
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'inferior-emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font `("MonacoB2"
                               :size 13
                               :weight bold
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq exec-path-from-shell-check-startup-files nil))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Include the env var from `.profile'
  (include-shell-var-in "~/.profile")

  ;; Settings for theme
  (cl-case (first dotspacemacs-themes)
    ('spacemacs-dark
     (custom-set-faces
      '(rainbow-delimiters-depth-1-face ((t :foreground "#3f78ac")))
      '(rainbow-delimiters-depth-2-face ((t :foreground "#96589d")))
      '(rainbow-delimiters-depth-3-face ((t :foreground "#24775c")))
      '(rainbow-delimiters-depth-4-face ((t :foreground "#528d17")))
      '(rainbow-delimiters-depth-5-face ((t :foreground "#8d7717")))
      '(rainbow-delimiters-depth-6-face ((t :foreground "#376996")))
      '(rainbow-delimiters-depth-7-face ((t :foreground "#834d89")))
      '(rainbow-delimiters-depth-8-face ((t :foreground "#1f6851")))
      '(rainbow-delimiters-depth-9-face ((t :foreground "#7c918a")))))
    ('spacemacs-light
     (custom-set-faces
      '(font-lock-function-name-face    ((t :foreground nil
                                            :background "#eff2fb"
                                            :inherit font-lock-variable-name-face)))
      '(font-lock-regexp-grouping-backslash ((t :background "#eaf4f1")))
      '(font-lock-regexp-grouping-construct ((t :background "#eaf4f1")))
      '(rainbow-delimiters-depth-1-face ((t :foreground "#619acf")))
      '(rainbow-delimiters-depth-2-face ((t :foreground "#895a82")))
      '(rainbow-delimiters-depth-3-face ((t :foreground "#56aa8f")))
      '(rainbow-delimiters-depth-4-face ((t :foreground "#85c04a")))
      '(rainbow-delimiters-depth-5-face ((t :foreground "#c0aa4a")))
      '(rainbow-delimiters-depth-6-face ((t :foreground "#3a81c3")))
      '(rainbow-delimiters-depth-7-face ((t :foreground "#986e91")))
      '(rainbow-delimiters-depth-8-face ((t :foreground "#6cb49d")))
      '(rainbow-delimiters-depth-9-face ((t :foreground "#9f8c8c"))))))
  (let ((height (face-attribute 'default :height)))
    (custom-set-faces
     `(linum                       ((t :underline nil :height ,height)))
     `(linum-relative-current-face ((t :underline nil :height ,height)))
     '(lazy-highlight              ((t :underline t :weight bold)))
     '(markdown-line-break-face    ((t :underline (:color foreground-color :style wave)
                                       :inherit shadow)))))
  (add-hook 'after-make-frame-functions
            (lambda (&rest _)
              (interactive)
              (spacemacs/load-theme (first dotspacemacs-themes))
              (spacemacs//show-trailing-whitespace)))

  ;; Settings for frame
  (add-hook 'after-make-frame-functions #'move-frame-to-right 'append)

  ;; Settings for macOS
  (when (eq system-type 'darwin)
    (setq ns-antialias-text t
          ns-pop-up-frames nil
          ns-command-modifier nil
          mac-option-modifier 'meta
          mac-pass-control-to-system nil
          mac-pass-command-to-system nil)
    (set-file-name-coding-system 'utf-8-hfs))

  ;; Settings for key-bind
  (evil-global-set-key 'insert (kbd "C-h") #'delete-backward-char)
  ;; HHKB
  (evil-global-set-key 'insert (kbd "<S-kp-divide>")   (kbd "\\"))
  (evil-global-set-key 'insert (kbd "<S-kp-subtract>") (kbd "_"))
  (evil-global-set-key 'insert (kbd "<S-kp-add>")      (kbd "="))

  ;; Settings for Hangul
  (evil-global-set-key 'normal (kbd "S-SPC") #'toggle-input-method)
  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (set-fontset-font t 'hangul (font-spec :name "NanumBarunGothicOTF"))
  (-update-var->> input-method-alist
                  (--map-when (string-equal "korean-hangul" (first it))
                              (-replace-at 3 "Hangul" it)))

  ;; Settings for pos/size of initial frame
  (let* ((w (if (<= 1440 (display-pixel-height)) 120 110))
         (h (1- (/ (custom-display-pixel-width) (frame-char-height))))
         (l (/ (custom-display-pixel-width) 2.0))
         (l (floor (- l (/ (frame-unit->pixel w) 2.8))))
         (l (if (< 0 (- (custom-display-pixel-width)
                        (+ l (frame-unit->pixel w))))
                l
              (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
    (add-to-list 'default-frame-alist (cons 'width  w))
    (add-to-list 'default-frame-alist (cons 'height h))
    (setq initial-frame-alist (list (cons 'top    0)
                                    (cons 'left   l)
                                    (cons 'width  w)
                                    (cons 'height h))))

  ;; Settings for scratch buffer
  (add-hook 'inferior-emacs-lisp-mode-hook #'spacemacs/toggle-smartparens-on)

  ;; Settings for minibuf
  (add-hook 'window-configuration-change-hook
            (lambda (&rest _)
              (ignore-errors
                (setq max-mini-window-height
                      (-some-> (window-list)
                               (-some->> (-map #'window-height)
                                         (-sort #'<))
                               (first)
                               (float)
                               (/ (frame-height))
                               (* (get-default 'max-mini-window-height)))))))

  ;; Settings for `ediff'
  (add-hook 'ediff-before-setup-hook #'spacemacs/toggle-maximize-frame-on)
  (advice-add #'ediff-quit
              :after (lambda (&rest _)
                       (spacemacs/toggle-maximize-frame-off)))

  ;; Settings for `aggressive-indent'
  (add-hook
   'spacemacs-buffer//startup-hook
   (when (require 'aggressive-indent nil 'noerr)
     (let ((agg-indent-defn (lambda (&rest _)
                              (unless (apply #'derived-mode-p
                                             aggressive-indent-excluded-modes)
                                (save-match-data
                                  (ignore-errors
                                    (aggressive-indent-indent-defun)))))))
       (add-hook 'evil-insert-state-exit-hook agg-indent-defn)
       (advice-add #'evil-paste-after :after agg-indent-defn)
       (advice-add #'evil-join :after agg-indent-defn)
       (advice-add #'evil-delete :after agg-indent-defn))))

  ;; Settings for `linum'
  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (< (buffer-size) (* 1024 50)))
                (linum-relative-mode))))

  ;; Settings for `company'
  (eval-after-load 'company
    '(progn
       (define-key company-active-map [remap company-show-doc-buffer] #'company-show-doc-buffer)
       (define-key company-active-map (kbd "C-h") nil)))

  ;; Settings for `helm'
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-h") #'delete-backward-char))
  (setq helm-autoresize-max-height 20
        helm-truncate-lines t)

  ;; Settings for `minibuf'
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "C-h")   #'backward-delete-char)
              (local-set-key (kbd "S-SPC") #'toggle-input-method)))

  ;; Settings for `ahs'
  (setq ahs-include '((clojure-mode . "[^ \t\n]+?")
                      (clojurescript-mode . "[^ \t\n]+?")
                      (clojurec-mode . "[^ \t\n]+?")
                      (emacs-lisp-mode . "[^ \t\n]+?")))

  ;; Settings for `magit'
  (setq magit-diff-refine-hunk t)
  (eval-after-load 'magit
    '(progn
       ;; Additional options that not included Magit
       ;;  This option is experimental feature
       ;;  It's available v2.9 and upper.
       ;;  See, http://www.spinics.net/lists/git/msg278919.html
       (add-to-list 'magit-diff-section-arguments "--compaction-heuristic")))
  (remove-hook 'magit-mode-hook #'turn-on-magit-gitflow)

  ;; Settings for `emacs-lisp'
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
      1 '(:inherit default) nil)
     ("(\\(lexical-let\\*?\\)"
      1 '(:inherit font-lock-keyword-face))
     ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
      1 '(:inherit font-lock-constant-face))
     ("(\\(assert\\)"
      1 '(:inherit font-lock-warning-face))
     (" \\(\\?.\\)"
      1 '(:inherit font-lock-string-face))
     ("\\(:[-+*/?0-9A-Za-z]+\\)"
      1 '(:inherit font-lock-constant-face))))

  ;; Settings for `smartparens'
  (advice-add #'sp-forward-symbol :before #'wrap-sp-forward-symbol)
  (advice-add #'sp-backward-symbol :after #'wrap-sp-backward-symbol)
  (advice-add #'sp-forward-sexp :after #'wrap-sp-forward-sexp)
  (advice-add #'sp-backward-sexp :after #'wrap-sp-backward-sexp)

  ;; Settings for `java'
  (setq eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse")
        eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")

  ;; Settings for `clojure'
  (require 'smartparens-clojure)
  (setq clojure-indent-style :align-arguments
        cider-mode-line ""
        cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-repl-use-pretty-printing t
        cljr-expectations-test-declaration "[expectations :refer :all]")
  (add-hook 'cider-repl-mode-hook #'spacemacs/toggle-smartparens-on)
  (evil-define-key 'insert cider-repl-mode-map (kbd "RET") #'evil-ret-and-indent)
  (evil-define-key 'normal cider-repl-mode-map (kbd "RET") #'cider-repl-return)
  (evil-define-key 'insert clojure-mode-map (kbd "/")
    (lambda ()
      (interactive)
      (when (prog1 (->> (char-before)
                        (char-to-string)
                        (string-match-p "[^ \t\n]"))
              (insert "/"))
        (company-complete-common-or-cycle))))
  (eval-after-load 'clj-refactor
    '(diminish 'clj-refactor-mode))
  (dolist (mode clojure-modes)
    (add-to-list 'page-break-lines-modes mode)
    (font-lock-add-keywords
     mode
     '(("[^ \t\n]+?\\(/\\)[^ \t\n]"
        1 '(:inherit shadow))
       ("\\s(\\(?:[^ \t\n]+?/\\)?\\(default[^ \t\n]*?\\)[ \t\n]+\\([^ \t\n]+?\\)"
        (1 '(:inherit default))
        (2 '(:inherit default)))
       ("\\s(\\(\\(?:as\\|some\\)?->>?\\|and\\|or\\)\\_>"
        1 '(:inherit default))
       ("^\\s-*\\s(def-\\s-+\\([^ \t\n]+\\)"
        1 '(:inherit font-lock-variable-name-face) nil)
       ("\\s([^ \t\n]+\\(!+\\)"
        1 '(:inherit font-lock-warning-face :slant italic :weight normal))
       ("\\(#js\\)\\s-+\\s("
        1 '(:inherit font-lock-builtin-face))
       ("\\_<\\(\\.-?\\)[a-z][a-zA-Z0-9]*\\_>"
        1 '(:inherit font-lock-keyword-face))
       ("\\_<\\(try\\+\\)\\_>"
        1 '(:inherit font-lock-keyword-face)))))
  (put 'def- 'clojure-doc-string-elt 2)
  (put 'defmacro- 'clojure-doc-string-elt 2)

  ;; Settings for `elixir'
  (evil-define-key 'insert elixir-mode-map (kbd "TAB")
    (lambda ()
      (interactive)
      (if (and (string-match-p "[^ \t\n]" (char-to-string (char-before)))
               (string-match-p  "[ \t\n]" (char-to-string (char-after))))
          (company-complete-common-or-cycle)
        (indent-for-tab-command))))

  ;; Settings for `latex'
  (add-hook 'LaTeX-mode-hook #'latex-preview)
  (add-hook 'LaTeX-mode-hook #'page-break-lines-mode)
  (add-hook 'LaTeX-mode-hook #'spacemacs/toggle-smartparens-on)
  (eval-after-load 'doc-view
    '(progn
       (-update-var->> doc-view-ghostscript-options
                       (--remove (string-match-p "-sDEVICE=.*" it))
                       (append '("-sDEVICE=pngalpha")))))

  ;; Settings for `org'
  (font-lock-add-keywords
   'org-mode
   '(("^\\s-*\\(-\\) "
      1 (compose-region (match-beginning 1) (match-end 1) ?∙))) t)
  (setq org-directory (concat (getenv "HOME")
                              "/Library/Mobile\ Documents"
                              "/com\~apple\~CloudDocs/Org")
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-src-fontify-natively t
        org-startup-indented t
        org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌"))

  ;; Settings for `markdown'
  (font-lock-add-keywords
   'markdown-mode
   '(("\\(<[^>]+>\\)"
      1 '(:inherit shadow))))
  (sp-local-pair 'markdown-mode "<" ">")
  )


;; Help functions for user-config

(defmacro -update-var->> (&rest thread)
  `(setq ,(first thread) (->> ,@thread)))

(defun get-default (var)
  (first (or (get var 'standard-value)
             (get var 'saved-value)
             (get var 'customized-value))))

(defun pixel->frame-unit (pixel)
  (round (/ pixel (/ (float (frame-pixel-width)) (frame-width)))))
(defun frame-unit->pixel (frame-unit)
  (round (* frame-unit (/ (float (frame-pixel-width)) (frame-width)))))
(defun custom-display-pixel-width ()
  (->> (--filter (-when-let (frames (-> (assoc 'frames it) cdr))
                   (--some? (eq (selected-frame) it) frames))
                 (display-monitor-attributes-list))
       (first)
       (assoc 'geometry)
       (cdr)
       (nth 2)))

(defun latex-build-tex (&optional file)
  (interactive)
  (let ((file (or file buffer-file-name)))
    (when (and (file-exists-p file)
               (= 0 (call-process "pdflatex" nil nil nil file)))
      (concat (file-name-sans-extension file) ".pdf"))))
(defun latex-preview (&rest _)
  (-when-let (pdf (latex-build-tex))
    (save-window-excursion
      (if (->> (frame-parameters)
               (assoc 'fullscreen)
               (cdr))
          (find-file-other-window pdf)
        (find-file-other-frame pdf))))
  (add-hook 'after-save-hook
            (lambda (&rest _)
              (message "Build & Reload PDF file")
              ;; TODO:
              ;; execute asynchronously
              (-when-let (pdf (latex-build-tex))
                (-when-let (buf (->> (frame-list)
                                     (--mapcat (window-list it))
                                     (--map (window-buffer it))
                                     (--filter (string-equal pdf (buffer-file-name it)))
                                     (-first-item)))
                  (save-window-excursion
                    (set-buffer buf)
                    (doc-view-revert-buffer nil t)))))
            nil t))

(defvar clojure-modes '(clojure-mode cider-repl-mode emacs-lisp-mode))
(defun wrap-sp-forward-symbol (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p clojure-modes)
               (-some->> (buffer-substring-no-properties (point) (line-end-position))
                         (string-match (concat "^\\s-*" sp-clojure-prefix "[^({\\[]"))))
      (goto-char (+ (point) (match-end 0))))))
(defun wrap-sp-backward-symbol (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p clojure-modes)
               (-some->> (buffer-substring-no-properties (line-beginning-position) (point))
                         (string-match (concat sp-clojure-prefix "\\s-*$"))))
      (beginning-of-line)
      (goto-char (+ (point) (match-beginning 0))))))
(defun wrap-sp-forward-sexp (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p clojure-modes)
               (-some->> (char-after)
                         (char-to-string)
                         (string-match "\\s(")))
      (forward-sexp))))
(defun wrap-sp-backward-sexp (&optional arg)
  (save-match-data
    (when (and (numberp arg)
               (> arg 0)
               (apply #'derived-mode-p clojure-modes)
               (-some->> (buffer-substring-no-properties (line-beginning-position) (point))
                         (string-match (concat sp-clojure-prefix "\\s-*$"))))
      (beginning-of-line)
      (goto-char (+ (point) (match-beginning 0))))))

(defun resolve-sh-var (str)
  (while (string-match (concat "\\$\\([_a-zA-Z0-9]+\\|[({].+[})]\\)") str)
    (let* ((var (match-string 1 str))
           (res (save-match-data
                  (->> var
                       (concat "echo $")
                       (shell-command-to-string)
                       (s-trim)))))
      (setq str (replace-match res t nil str))))
  str)
(defun include-shell-var-in (file)
  (when (file-exists-p file)
    (let* ((regx "export\\s-+\\([^=]+\\)=\"?\\(.+?\\)\"?$")
           (exports (->> (with-temp-buffer
                           (insert-file-contents file)
                           (split-string (buffer-string) "\n" t))
                         (--filter (not (string-match-p "^#" it)))
                         (--filter (string-match-p regx it))
                         (--map (replace-regexp-in-string "\\\\" "" it)))))
      (dolist (it exports)
        (string-match regx it)
        (let ((key   (match-string-no-properties 1 it))
              (value (match-string-no-properties 2 it)))
          (setenv key (resolve-sh-var value)))))))

(defun frame-x (frame)
  (cdr (assoc 'left (frame-parameters frame))))
(defun frame-y (frame)
  (cdr (assoc 'top (frame-parameters frame))))
(defun rightmost-frame ()
  (-some->> (visible-frame-list)
            (--sort (> (frame-x it) (frame-x other)))
            (-first-item)))
(defun move-frame-to-right (frame)
  (let* ((step (frame-unit->pixel 30))
         (x (+ (frame-x (rightmost-frame)) step))
         (x (if (< (custom-display-pixel-width) (+ x (frame-pixel-width frame)))
                (- (custom-display-pixel-width) (frame-pixel-width frame) 1)
              x)))
    (set-frame-position frame x (frame-y frame))))

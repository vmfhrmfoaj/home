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
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-private")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     (c-c++ :variables c-c++-enable-clang-support t)
     clojure
     emacs-lisp
     git
     (gtags :variables gtags-enable-by-default nil)
     helm
     html
     java
     javascript
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill nil)
     markdown
     org
     osx
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     =enable-flyspell-auto-completion= t)
     swift
     themes-megapack
     version-control
     yaml
     ;; ---------------------------------------------------------------
     ;; Extentions
     ;; ---------------------------------------------------------------
     auto-completion-ext
     clojure-ext
     emacs-lisp-ext
     git-ext
     gtags-ext
     helm-ext
     html-ext
     java-ext
     javascript-ext
     org-ext
     spacemacs-base-ext
     spacemacs-bootstrap-ext
     spacemacs-editing-ext
     spacemacs-editing-visual-ext
     spacemacs-evil-ext
     version-control-ext
     ;; ---------------------------------------------------------------
     ;; Private layers
     ;; ---------------------------------------------------------------
     exercise
     eye-candy
     (focus :variables
            focus-mode-to-new-thing '((cider-repl-mode . list+)
                                      (clojure-mode    . list+)
                                      (emacs-lisp-mode . list+))))
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(exec-path-from-shell)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

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
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
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
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(twilight-bright)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; If you used macOS, you can control advance setting of fonts.
   ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 1~3
   ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 1~16
   dotspacemacs-default-font `("Fira Code"
                               :size ,(if (<= 2560 (car (cdddar (frame-monitor-attributes))))
                                          15 ; retina display
                                        14)
                               :weight light
                               :width normal
                               :powerline-scale 1.3)
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
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
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
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; load custom functions
  (load "~/.spacemacs-funcs.el")

  ;; set the `custom-file' to avoid appending tail...
  (setq custom-file "~/.spacemacs-custom.el")
  (load custom-file)

  ;; user info
  (setq user-full-name "Jinseop Kim"
        user-mail-address "vmfhrmfoaj@yahoo.com")

  ;; set up the addtional font setting.
  (set-fontset-font t 'hangul (font-spec :name "Nanum Gothic"))
  (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95))
  (setq-default line-spacing 1)
  (mac-auto-operator-composition-mode))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; include the ".profile" file for the GUI emacs.
  (when window-system
    (include-shell-var-in "~/.profile"))

  ;; korean
  (set-language-environment "Korean")
  (-update->> input-method-alist
              (--map-when (string-equal "korean-hangul" (first it))
                          (-replace-at 3 "Hangul" it)))
  (set-file-name-coding-system 'utf-8-hfs)
  (prefer-coding-system 'utf-8)
  (global-set-key (kbd "S-SPC") #'toggle-input-method)

  ;; mac
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier  'meta
          mac-pass-control-to-system nil
          mac-pass-command-to-system t)
    (set-file-name-coding-system 'utf-8-hfs))

  ;; HHKB keyboard
  (global-set-key (kbd "<S-kp-divide>") "\\")
  (global-set-key (kbd "<S-kp-subtract>") "_")
  (global-set-key (kbd "<S-kp-add>") "=")
  (define-key key-translation-map (kbd "<S-kp-divide>") "\\")
  (define-key key-translation-map (kbd "<S-kp-subtract>") "_")
  (define-key key-translation-map (kbd "<S-kp-add>") "=")

  (when window-system
    ;; for single window
    (let* ((w 130)
           (h (1- (/ (display-pixel-height) (frame-char-height))))
           (l (/ (custom-display-pixel-width) 2.0))
           (l (floor (- l (* (frame-unit->pixel w) 0.45))))
           (l (if (< 0 (- (custom-display-pixel-width)
                          (+ l (frame-unit->pixel w))))
                  l
                (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w)))))
           (w (min w (pixel->frame-unit (- (custom-display-pixel-width) l 120))))
           (w (max w 100)))
      (add-to-list 'default-frame-alist (cons 'width  w))
      (add-to-list 'default-frame-alist (cons 'height h))
      (setq org-tags-column (* -1 (- w 10))
            split-width-threshold (1+ w)
            initial-frame-alist (list (cons 'top    0)
                                      (cons 'left   l)
                                      (cons 'width  w)
                                      (cons 'height h))))

    ;; for fullscreen
    ;; (toggle-frame-maximized)
    ;; (dotimes (i (1- (/ (custom-display-pixel-width) (frame-char-width) 120)))
    ;;   (split-window-right))
    ;; (require 'dash-functional)
    ;; (defun set-window-buffer+ (set-win-buf wind buf &optional opt)
    ;;   (when (and (->> (window-list)
    ;;                   (-remove (-partial #'eq (selected-window)))
    ;;                   (-map #'window-buffer)
    ;;                   (-some? (-partial #'eq buf)))
    ;;              (->> this-command
    ;;                   (format "%s")
    ;;                   (string-match-p "quit\\|bury")
    ;;                   (not)))
    ;;     (funcall set-win-buf
    ;;              (->> (window-list)
    ;;                   (--remove (eq (selected-window) it))
    ;;                   (--filter (eq buf (window-buffer it)))
    ;;                   (-first-item))
    ;;              (window-buffer wind) opt))
    ;;   (funcall set-win-buf wind buf opt))
    ;; (advice-add 'set-window-buffer :around #'set-window-buffer+)
    ;; (eval-after-load "helm-buffers"
    ;;   '(progn
    ;;      (defvar helm-source-window-buffers-list
    ;;        (helm-build-sync-source "Window buffers"
    ;;          :action #'switch-to-buffer
    ;;          :real-to-display (-compose (-partial #'apply #'concat)
    ;;                                     #'helm-buffer--details)
    ;;          :candidates (let ((byte-compile-warnings nil)
    ;;                            (byte-compile-dynamic t)
    ;;                            (f (lambda ()
    ;;                                 (->> (window-list)
    ;;                                      (-remove (-partial #'eq (selected-window)))
    ;;                                      (-map #'window-buffer)
    ;;                                      (-distinct)))))
    ;;                        (byte-compile f))))
    ;;      (add-to-list 'helm-mini-default-sources
    ;;                   'helm-source-window-buffers-list)))
    )

  ;; large file
  (add-hook 'find-file-hook
            (lambda ()
              (when (>= (buffer-size) (* 1024 1024))
                (prettify-symbols-mode -1)
                (turn-off-show-smartparens-mode)
                (turn-off-smartparens-mode)))
            'append)

  ;; for improving performance.
  (setq garbage-collection-messages nil
        gc-cons-threshold (* 64 1024 1024))
  (run-with-idle-timer 1 t #'garbage-collect)


  (let ((f (lambda (&rest _)
             (->> (face-list)
                  (--filter (eq 'normal (face-attribute it :weight)))
                  (--map (set-face-attribute it nil :weight 'light))))))
    (add-hook 'after-load-functions f)
    (funcall f))
  ;; customize the theme.
  (custom-theme-set-faces
   'twilight-bright
   `(hl-line ((t (:background "#fdeeee"))))
   `(magit-section-highlight ((t (:inherit hl-line))))
   `(org-block ((t (:foreground "#4d4d4d" :background "#fcfcfc" :slant normal :inherit org-meta-line))))
   `(org-link  ((t (:inherit link))))
   `(org-tag   ((t (:weight light :underline t))))
   `(outline-4 ((t (:inherit font-lock-string-face))))
   `(region ((t (:background "#fcdfdf")))))
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t :foreground ,(-> 'default (face-attribute :foreground) (light-color 2))
                                     :background ,(-> 'default (face-attribute :background) (dim-color 3)))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(css-property ((t (:inherit font-lock-builtin-face :foreground nil :weight light))))
   `(css-selector ((t (:inherit font-lock-variable-name-face :foreground nil :weight bold))))
   `(git-timemachine-minibuffer-detail-face ((t (:foreground nil :inherit highlight))))
   `(fringe ((t (:background ,(-> 'default (face-attribute :background) (dim-color 1))))))
   `(font-lock-function-name-face ((t (:inherit bold))))
   `(font-lock-variable-name-face ((t (:inherit bold))))
   `(font-lock-type-face ((t (:weight light))))
   `(font-lock-comment-face ((t (:slant normal))))
   `(link ((t (:underline t))))
   `(linum ((t (:inherit default))))
   `(linum-relative-current-face ((t (:foreground ,(face-attribute 'default :foreground) :inherit linum))))
   `(mode-line ((t (:distant-foreground ,(face-attribute 'mode-line :foreground)))))
   `(mode-line-inactive ((t (:distant-foreground ,(face-attribute 'mode-line-inactive :foreground)))))
   `(org-agenda-current-time (( t (:foreground "#2d9574" :height 0.9))))
   `(org-agenda-date ((t (:inherit (font-lock-variable-name-face org-agenda-structure)))))
   `(org-agenda-date-today ((t (:inherit (font-lock-function-name-face org-agenda-structure)))))
   `(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
   `(org-agenda-done ((t (:height 1.0 :inherit bold))))
   `(org-agenda-structure ((t (:height 1.3))))
   `(org-hide ((t :background ,(face-attribute 'default :background)
                  :foreground ,(face-attribute 'default :background))))
   `(org-cancelled ((t (:foreground nil :inherit org-done))))
   `(org-document-title ((t (:family ,(first dotspacemacs-default-font) :height 1.4))))
   `(org-next ((t (:foreground "#dca3a3" :weight bold :inherit org-todo))))
   `(org-time-grid ((t :foreground ,(-> 'default
                                        (face-attribute :foreground)
                                        (light-color 50))
                       :height 0.9)))
   `(show-paren-match ((t (:background "#eefff6" :foreground "Springgreen2" :underline t :weight bold))))
   `(shadow ((t (:foreground ,(-> 'default (face-attribute :foreground) (light-color 30)))))))
  (with-eval-after-load "goto-addr"
    (setq goto-address-mail-face "link"))
  (with-eval-after-load "powerline"
    (set-face-attribute 'powerline-active1 nil :foreground "#85ceeb")
    (set-face-attribute 'powerline-active2 nil :foreground "#85ceeb"))
  (with-eval-after-load "org"
    (dolist (i (number-sequence 1 org-n-level-faces))
      (set-face-attribute (intern (concat "org-level-" (number-to-string i))) nil :weight 'bold)))
  (with-eval-after-load "rainbow-delimiters"
    (dolist (i (number-sequence 1 9))
      (let ((face (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))))
        (set-face-attribute face nil :foreground
                            (light-color (face-attribute face :foreground) 15)))))

  ;; for programming
  (add-to-list 'auto-mode-alist '("\\.m\\s-*$" . objc-mode))
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               `(("\\('\\|`\\|,\\|@\\|#\\|~\\|\\^\\|_\\|\\s(\\|\\s)\\|[{}]\\)"
                  1 'shadow)
                 ("[\[ \r\t\n]\\(&\\)[ \r\t\n]"
                  1 'shadow)
                 ("[^ \r\t\n]\\(/\\)[^ \r\t\n]"
                  1 'shadow t))
               t)))

  ;; for org-capture Chrome extension
  (require 'org-protocol)

  ;; turn on/off the packages globally.
  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; cleanup
  (eval-after-load "projectile"
    '(projectile-cleanup-known-projects))

  (eval-after-load "recentf"
    '(recentf-cleanup)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

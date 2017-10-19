;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
     c-c++
     clojure
     csv
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
     (markdown :variables markdown-live-preview-engine 'vmd)
     org
     osx
     python
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     =enable-flyspell-auto-completion= t)
     swift
     version-control
     yaml
     ;; ---------------------------------------------------------------
     ;; Extentions
     ;; ---------------------------------------------------------------
     auto-completion-ext
     c-c++-ext
     clojure-ext
     emacs-lisp-ext
     git-ext
     gtags-ext
     helm-ext
     html-ext
     java-ext
     javascript-ext
     latex-ext
     org-ext
     spacemacs-ext
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
   dotspacemacs-excluded-packages '(exec-path-from-shell evil-escape)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(twilight-bright)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; Options(only available on macOS):
   ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 1~3
   ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 0~16
   dotspacemacs-default-font (let ((linux? (eq system-type 'gnu/linux)))
                               `("MonacoB2"
                                 :size ,(cond
                                         (linux? 12)
                                         (t 14))
                                 :weight normal
                                 :width normal
                                 :powerline-scale ,(cond
                                                    (linux? 1.15)
                                                    (t 1.1))))
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
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
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize t
   ;; if non-nil, the helm header is hidden when there is only one source.
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
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
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
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; compile settings
  (setq-default byte-compile-dynamic t
                byte-compile-warnings nil)

  ;; load custom functions
  (load "~/.spacemacs-funcs.el")

  ;; set the `custom-file' to avoid appending tail...
  (setq custom-file "~/.spacemacs-custom.el")
  (load custom-file)

  ;; enable the `dir-local-variable' on remote
  (setq enable-remote-dir-locals t)

  ;; clean up list of recently visited file and project
  (add-hook 'emacs-startup-hook
            (lambda ()
              (recentf-cleanup)
              (projectile-cleanup-known-projects))))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; user info
  (setq user-full-name "Jinseop Kim"
        user-mail-address "vmfhrmfoaj@yahoo.com")

  ;; set up the addtional font setting
  (setq-default line-spacing 0)
  (set-fontset-font t 'hangul (font-spec :name "Nanum Gothic"))
  (add-to-list 'face-font-rescale-alist '("Arial Unicode MS" . 0.95))
  (add-to-list 'face-font-rescale-alist '("STIXGeneral" . 0.9))
  (let ((font (car dotspacemacs-default-font))
        (size (plist-get (cdr dotspacemacs-default-font) :size))
        (linux? (eq system-type 'gnu/linux))
        (mac? (eq system-type 'darwin)))
    (cond
     ((string-equal font "Fira Code")
      (setq-default line-spacing 1)
      (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.95)))
     ((string-equal font "MonacoB2")
      (cond
       ((and linux? (not (= 13 size)))
        (add-to-list 'face-font-rescale-alist '("Fira Code Symbol" . 0.95)))
       ((and mac? (= 13 size))
        (setq-default line-spacing 1)
        (add-to-list 'face-font-rescale-alist '("Fira Code Symbol" . 1.1)))))
     ((string-equal font "Fantasque Sans Mono")
      (add-to-list 'face-font-rescale-alist '("Nanum Gothic" . 0.85))
      (add-to-list 'face-font-rescale-alist '("Fira Code Symbol" . 0.9))
      (add-to-list 'face-font-rescale-alist '("Arial" . 0.9))
      (add-to-list 'face-font-rescale-alist '("Andale Mono" . 0.9))
      (add-to-list 'face-font-rescale-alist '("all-the-icons" . 0.9))
      (add-to-list 'face-font-rescale-alist '("file-icons" . 0.9))
      (add-to-list 'face-font-rescale-alist '("FontAwesome" . 0.9))
      (add-to-list 'face-font-rescale-alist '("github-octicons" . 0.9))
      (add-to-list 'face-font-rescale-alist '("Weather Icons" . 0.9))
      (add-to-list 'face-font-rescale-alist '("Material Icons" . 0.9)))))
  (when (string-equal "Fira Code" (car dotspacemacs-default-font))
    (let ((alist '(( 33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
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
                   ( 63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                   ( 91 . ".\\(?:]\\)")
                   ( 92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                   ( 94 . ".\\(?:=\\)")
                   (119 . ".\\(?:ww\\)")
                   (123 . ".\\(?:-\\)")
                   (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                   (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-function-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring])))))

  ;; include the ".profile" file for the GUI emacs.
  (when window-system
    (include-shell-var-in "~/.profile"))

  ;; Korean
  (set-language-environment "Korean")
  (-update->> input-method-alist
              (--map-when (string-equal "korean-hangul" (first it))
                          (-replace-at 3 "Hangul" it)))
  (global-set-key (kbd "S-SPC") #'toggle-input-method)

  ;; UTF-8
  (prefer-coding-system 'utf-8)

  ;; macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier  'meta
          mac-pass-control-to-system nil
          mac-pass-command-to-system t)
    (require 'ucs-normalize)
    (set-file-name-coding-system 'utf-8-hfs))

  ;; HHKB keyboard
  (global-set-key (kbd "<S-kp-divide>") "\\")
  (global-set-key (kbd "<S-kp-subtract>") "_")
  (global-set-key (kbd "<S-kp-add>") "=")
  (define-key input-decode-map (kbd "<S-kp-divide>") "\\")
  (define-key input-decode-map (kbd "<S-kp-subtract>") "_")
  (define-key input-decode-map (kbd "<S-kp-add>") "=")

  ;; multiple window
  (advice-add 'set-window-buffer :around #'set-window-buffer+)

  ;; window pos & size
  (when window-system
    (let ((w 150))
      (setq org-tags-column (+ (- w) 5))
      (if (or dotspacemacs-fullscreen-at-startup
              dotspacemacs-fullscreen-use-non-native
              dotspacemacs-maximized-at-startup)
          (dotimes (i (1- (/ (custom-display-pixel-width) (frame-char-width) w)))
            (split-window-right))
        (let* ((h (- (/ (display-pixel-height) (frame-char-height))
                     (if (zerop (% (display-pixel-height) (frame-char-height)))
                         2 1)
                     2))
               (l (/ (custom-display-pixel-width) 2.0))
               (l (floor (- l (* (frame-unit->pixel w) 0.5))))
               (l (if (< 0 (- (custom-display-pixel-width)
                              (+ l (frame-unit->pixel w))))
                      l
                    (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w))))))
          (add-to-list 'default-frame-alist (cons 'width  w))
          (add-to-list 'default-frame-alist (cons 'height h))
          (setq split-width-threshold (1+ w)
                initial-frame-alist (list (cons 'top    (frame-char-height))
                                          (cons 'left   l)
                                          (cons 'width  w)
                                          (cons 'height h)))))))

  ;; large file
  (add-hook 'find-file-hook
            (byte-compile
             (lambda ()
               (when (>= (buffer-size) (* 1024 1024))
                 (prettify-symbols-mode -1)
                 (turn-off-show-smartparens-mode)
                 (turn-off-smartparens-mode))))
            'append)

  ;; customize the theme.
  (custom-theme-set-faces
   'twilight-bright
   `(auto-dim-other-buffers-face
     ((t :foreground  ,(-> 'default (face-attribute :foreground) (light-color 5))
         :background  ,(-> 'default (face-attribute :background) (dim-color 3)))))
   `(clojure-if-true-face
     ((t (:background ,(-> 'font-lock-keyword-face
                           (face-attribute :background)
                           (light-color 2.5))))))
   `(clojure-fn-parameter-face ((t (:inherit font-lock-variable-name-face :weight normal))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-regexp-grouping-construct))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold :foreground ,(-> 'font-lock-string-face
                                                                            (face-attribute :foreground)
                                                                            (light-color 5)
                                                                            (saturate-color 10))))))
   `(fringe ((t (:background ,(-> 'default (face-attribute :background) (dim-color 1))))))
   `(git-gutter+-added    ((t (:foreground ,(face-attribute 'diff-refine-added   :background)))))
   `(git-gutter+-deleted  ((t (:foreground ,(face-attribute 'diff-refine-removed :background)))))
   `(git-gutter+-modified ((t (:foreground ,(face-attribute 'diff-refine-changed :background)))))
   `(git-timemachine-minibuffer-detail-face ((t (:foreground nil :inherit highlight))))
   `(hl-line ((t (:background "#eef7fd"))))
   `(linum-relative-current-face ((t (:inherit linum :foreground ,(face-attribute 'default :foreground)))))
   `(link ((t (:foreground "#55850f" :underline t))))
   `(magit-diff-context-highlight ((t (:background "#f2f9fd"))))
   `(magit-diff-hunk-heading-highlight ((t (:background "#c8e9ff"))))
   `(magit-section-highlight ((t (:background "#eef7fd"))))
   `(org-cancelled ((t (:foreground nil :inherit org-done))))
   `(org-column ((t (:weight bold))))
   `(org-hide ((t (:foreground ,(face-attribute 'default :background) :background unspecified))))
   `(org-link ((t (:inherit link))))
   `(org-next ((t (:foreground "#dca3a3" :weight bold :inherit org-todo))))
   `(org-checkbox  ((t (:weight bold))))
   `(org-next      ((t (:foreground "#dca3a3" :weight bold :inherit org-todo))))
   `(outline-4 ((t (:inherit font-lock-string-face))))
   `(show-paren-match ((t (:foreground "Springgreen2" :underline t :weight bold))))
   `(powerline-active1   ((t (:foreground "#85CEEB" :background "#383838" :inherit mode-line))))
   `(powerline-active2   ((t (:foreground "#85CEEB" :background "#6b6b6b" :inherit mode-line))))
   `(powerline-inactive1 ((t (:foreground "#F0F0EF" :background "#686868" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:foreground "#F0F0EF" :background "#A9A9A9" :inherit mode-line-inactive)))))
  (custom-set-faces
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-keyword-face   ((t (:inherit font-lock-builtin-face))))
   `(css-property ((t (:inherit font-lock-builtin-face :foreground nil :weight normal))))
   `(css-selector ((t (:inherit font-lock-variable-name-face :foreground nil :weight bold))))
   `(font-lock-comment-face ((t (:slant unspecified))))
   `(font-lock-doc-face ((t (:slant italic))))
   `(font-lock-function-name-face ((t (:inherit bold :weight unspecified))))
   `(font-lock-keyword-face ((t (:inherit nil :weight unspecified))))
   `(font-lock-type-face    ((t (:inherit nil :weight unspecified))))
   `(font-lock-variable-name-face ((t (:inherit bold :weight unspecified))))
   `(hl-line ((t (:inverse-video nil))))
   `(link ((t (:underline t :weight unspecified))))
   `(linum ((t (:inherit default :underline nil :height 1.0))))
   `(linum-relative-current-face ((t (:inherit linum))))
   `(mode-line ((t (:distant-foreground ,(face-attribute 'mode-line :foreground)))))
   `(mode-line-inactive ((t (:distant-foreground ,(face-attribute 'mode-line-inactive :foreground))))))
  (with-eval-after-load "goto-addr"
    (setq goto-address-mail-face "link"))
  (with-eval-after-load "highlight-parentheses"
    (setq hl-paren-colors (--iterate (dim-color it 10)
                                     (apply 'color-rgb-to-hex (color-name-to-rgb "Springgreen1"))
                                     4)))

  ;; for programming
  (add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\('\\|`\\|,\\|@\\|#\\|~\\|\\^\\|_\\|\\s(\\|\\s)\\|[{}]\\)"
                  1 'shadow)
                 ("[\[ \r\t\n]\\(&\\)[ \r\t\n]"
                  1 'shadow))
               t)
              (font-lock-add-keywords
               nil
               '(("[0-9A-Za-z]\\(/\\)[<>0-9A-Za-z]"
                  1 'shadow)))))

  ;; for org-capture Browser extension
  (require 'org-protocol)

  ;; turn on/off the packages globally.
  (spacemacs/toggle-camel-case-motion-globally-on)
  (spacemacs/toggle-mode-line-org-clock)
  (unless dotspacemacs-fullscreen-at-startup
    (spacemacs/toggle-golden-ratio-on)))

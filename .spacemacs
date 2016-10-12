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
     clojure
     emacs-lisp
     git
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
     themes-megapack
     version-control
     ;; ---------------------------------------------------------------
     ;; Extentions
     ;; ---------------------------------------------------------------
     base-ext
     clojure-ext
     emacs-lisp-ext
     git-ext
     html-ext
     java-ext
     minor-mode-extentions
     org-ext
     ;; ---------------------------------------------------------------
     ;; Private layers
     ;; ---------------------------------------------------------------
     auto-dim-other-buffer
     exercise
     eye-candy
     (focus :variables
            focus-mode-to-new-thing '((cider-repl-mode . list+)
                                      (clojure-mode    . list+)
                                      (emacs-lisp-mode . list+)))
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     exec-path-from-shell
     )
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
   dotspacemacs-themes '(default)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; If you used macOS, you can control advance setting of fonts.
   ;; - defaults write org.gnu.Emacs AppleFontSmoothing -int 1~3
   ;; - defaults write org.gnu.Emacs AppleAntiAliasingThreshold -int 1~16
   dotspacemacs-default-font '("MonacoB2"
                               :size 14
                               :weight normal
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
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq custom-file "~/.spacemacs-custom.el")

  ;; User info
  (setq user-full-name "Jinseop Kim"
        user-mail-address "vmfhrmfoaj@yahoo.com")

  ;; Setup the addtional font setting.
  (setq-default line-spacing 2)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Include the ".profile" file for the GUI emacs.
  (include-shell-var-in "~/.profile")

  ;; Setup language.
  (set-language-environment "Korean")
  (-update->> input-method-alist
              (--map-when (string-equal "korean-hangul" (first it))
                          (-replace-at 3 "Hangul" it)))
  (set-file-name-coding-system 'utf-8-hfs)
  (prefer-coding-system 'utf-8)

  ;; Mac
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'meta
          mac-pass-control-to-system nil
          mac-pass-command-to-system nil)
    (set-file-name-coding-system 'utf-8-hfs))

  ;; Setup the keys.
  (global-set-key (kbd "S-SPC") #'toggle-input-method)
  (global-set-key (kbd "<S-kp-divide>") "\\")
  (global-set-key (kbd "<S-kp-subtract>") "_")
  (global-set-key (kbd "<S-kp-add>") "=")
  (define-key evil-read-key-map (kbd "<S-kp-divide>") "\\")
  (define-key evil-read-key-map (kbd "<S-kp-subtract>") "_")
  (define-key evil-read-key-map (kbd "<S-kp-add>") "=")
  (define-key key-translation-map (kbd "<S-kp-divide>") "\\")
  (define-key key-translation-map (kbd "<S-kp-subtract>") "_")
  (define-key key-translation-map (kbd "<S-kp-add>") "=")
  (define-key isearch-mode-map (kbd "C-h") #'isearch-delete-char)
  (define-key isearch-mode-map (kbd "SPC") (gen-isearch-fn ".*?" " "))
  (define-key isearch-mode-map (kbd "M-<") (gen-isearch-fn "\\_<"))
  (define-key isearch-mode-map (kbd "M->") (gen-isearch-fn "\\_>"))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "C-h")   #'backward-delete-char)
              (local-set-key (kbd "S-SPC") #'toggle-input-method)))

  ;; TAB do not have intelligent behavior.
  (setq tab-always-indent t)
  (global-set-key (kbd "<S-tab>") #'completion-at-point)

  ;; Set the pos/size of the initial frame.
  (let* ((w 120)
         (h (1- (/ (display-pixel-height) (frame-char-height))))
         (l (/ (custom-display-pixel-width) 2.0))
         (l (floor (- l (* (frame-unit->pixel w) 0.3))))
         (l (if (< 0 (- (custom-display-pixel-width)
                        (+ l (frame-unit->pixel w))))
                l
              (max 0 (- (custom-display-pixel-width) (frame-unit->pixel w)))))
         (w (min w (pixel->frame-unit (- (custom-display-pixel-width) l 120))))
         (w (max w 100)))
    (add-to-list 'default-frame-alist (cons 'width  w))
    (add-to-list 'default-frame-alist (cons 'height h))
    (setq split-width-threshold (1+ w)
          initial-frame-alist (list (cons 'top    0)
                                    (cons 'left   l)
                                    (cons 'width  w)
                                    (cons 'height h))))

  ;; Setup "Fira Code Symbol".
  ;; NOTE
  ;; Use customized "Fira Code Symbol" font for "MonacoB2" as the default font.
  ;; - https://gist.github.com/mordocai/50783defab3c3d1650e068b4d1c91495
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  (add-hook 'prog-mode-hook
            (-partial #'font-lock-add-keywords nil
                      fira-code-font-lock-keywords-alist))
  (add-hook 'org-mode-hook
            (-partial #'font-lock-add-keywords nil
                      (-drop-last 1 fira-code-font-lock-keywords-alist)))

  ;; Change the behavior of indent function for `prettify-symbols-mode'.
  (advice-disable-modes '(prettify-symbols-mode) #'indent-for-tab-command)
  (advice-disable-modes '(prettify-symbols-mode) #'indent-region)
  (advice-disable-modes '(prettify-symbols-mode) #'indent-according-to-mode)

  ;; Customize the theme.
  (require 'color-theme-sanityinc-tomorrow)
  (color-theme-sanityinc-tomorrow--define-theme eighties)
  (add-hook 'linum-mode-hook (lambda () (set-face-attribute 'linum nil :background nil)))
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background "#242424"))))
   `(clojure-interop-method-face ((t (:inherit default))))
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(company-tooltip ((t (:background "#393939"))))
   `(company-tooltip-selection ((t (:background "#515151" :foreground "#99cc99" :weight bold))))
   `(css-property ((t (:inherit font-lock-builtin-face))))
   `(fic-face ((t (:inherit font-lock-warning-face :background nil :foreground nil :slant italic))))
   `(font-latex-bold-face ((t (:foreground nil :weight bold))))
   `(font-latex-italic-face ((t (:foreground nil :slant italic))))
   `(font-latex-math-face ((t (:inherit font-lock-variable-name-face :foreground nil))))
   `(font-latex-string-face ((t (:inherit font-lock-string-face))))
   `(font-latex-warning-face ((t (:inherit font-lock-warning-face :foreground nil))))
   `(font-lock-builtin-face ((t (:foreground "#d5aad5"))))
   `(font-lock-doc-face ((t (:foreground "#39acac" :slant italic))))
   `(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:weight bold))))
   `(fringe ((t (:background "#1d1f21"))))
   `(git-gutter+-added ((t (:foreground "#aad5aa"))))
   `(git-gutter+-deleted ((t (:foreground "#faa170"))))
   `(git-gutter+-modified ((t (:foreground "#ddbbdd"))))
   `(helm-selection ((t (:background ,(face-attribute 'highlight :background) :weight bold :inherit nil))))
   `(hl-paren-face ((t (:weight bold))))
   `(hl-todo ((t (:inherit font-lock-comment-face :foreground "#cc9393" :weight bold))))
   `(isearch ((t (:weight bold))))
   `(lazy-highlight ((t (:weight normal))))
   `(linum ((t (:inherit fringe :inverse-video nil :underline nil))))
   `(linum-relative-current-face ((t (:foreground "#cccccc" :weight bold :inherit linum))))
   `(mode-line-inactive ((t (:background "#313131" :foreground "#777777" :box (:line-width 1 :color "#777777")))))
   `(org-hide ((t (:inherit default :background nil :foreground nil))))
   `(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.2 :overline t))))
   `(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.0))))
   `(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.0))))
   `(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.0))))
   `(org-level-5 ((t (:inherit outline-5 :weight bold :height 1.0))))
   `(org-level-6 ((t (:inherit outline-6 :weight bold :height 1.0))))
   `(org-level-7 ((t (:inherit outline-7 :weight bold :height 1.0))))
   `(org-level-8 ((t (:inherit outline-8 :weight bold :height 1.0))))
   `(org-target ((t (:inherit font-lock-comment-face))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#a3a3a3"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#51a3a3"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#cca351"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#7aa37a"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#517aa3"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#8e8e8e"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#478e8e"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#b28e47"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#6b8e6b"))))
   `(show-paren-match ((t (:foreground nil :background nil :weight bold :underline t))))
   )

  ;; for programming modes.
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               `((,(concat "\\(\\s(\\|\\s)\\|'\\|`(\\|,\\|"
                           "\\\\\\|@\\|#\\|\\.\\|~\\|\\^\\)")
                  1 '(:inherit shadow))) t)))

  ;; Turn on some packages globally.
  (spacemacs/toggle-camel-case-motion-globally-on)
  (global-prettify-symbols-mode)
  (auto-dim-other-buffers-mode)
  )

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

(defmacro -update->> (&rest thread)
  `(setq ,(first thread) (-some->> ,@thread)))

(defun enabled? (mode-status)
  (cond ((symbolp mode-status) mode-status)
        ((numberp mode-status) (not (zerop mode-status)))
        (t nil)))
(defun disable-modes (modes)
  (--map (and (symbol-value it)
              (funcall it 0))
         modes))
(defun resotre-modes (modes status)
  (--map (and (cdr it)
              (funcall (car it) (cdr it)))
         (-zip modes status)))
(defmacro with-disable-modes (modes &rest body)
  `(let ((mode-status (-map #'symbol-value ,modes)))
     (disable-modes ,modes)
     (prog1 (progn ,@body)
       (resotre-modes ,modes mode-status))))
(put 'with-disable-modes 'lisp-indent-function 'defun)
(defun advice-disable-modes (modes f)
  (advice-add f :around
              (lexical-let ((modes modes))
                (lambda (f &rest args)
                  "Added by `advice-disable-modes'."
                  (with-disable-modes modes
                    (apply f args))))))

(defun gen-isearch-fn (regx &optional display-str)
  (lexical-let ((regx regx)
                (display-str (or display-str regx)))
    (lambda ()
      (interactive)
      (setq isearch-string  (concat isearch-string regx)
            isearch-message (concat isearch-message
                                    (mapconcat 'isearch-text-char-description
                                               display-str ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

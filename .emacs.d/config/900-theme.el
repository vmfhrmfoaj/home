;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package twilight-bright-theme
  :ensure t
  :defer t)

(use-package twilight-anti-bright-theme
  :ensure t
  :defer t)

(use-package theme-changer
  :disabled t
  :ensure t
  :config
  (defun theme-changer--custom-switch-theme (old new)
    "Fix for an error"
    (let ((new (if (listp new)
                   (elt new (random (length new)))
                 new))
          (enable (if (not (string= theme-changer-mode "deftheme"))
                      (lambda () (apply (symbol-function new) '()))
                    (lambda () (load-theme new t)))))
      (run-hook-with-args 'theme-changer-pre-change-functions old)
      (disable-theme old)
      (if new (funcall enable))
      (run-hook-with-args 'theme-changer-post-change-functions new)
      new))

  (setq calendar-location-name "South Korea"
        calendar-latitude   35.9078
        calendar-longitude 127.7669)

  (add-hook 'theme-changer-pre-change-functions
            (lambda (old-theme)
              (cond
               ((memq old-theme '(default twilight-bright))
                (setq frame-background-mode 'dark))
               ((memq old-theme '(twilight-anti-bright))
                (setq frame-background-mode 'light)))))

  (add-hook 'theme-changer-post-change-functions
            (lambda (_new-theme)
              (when (featurep 'powerline)
                (powerline-reset))))

  (advice-add #'theme-changer-switch-theme :override #'theme-changer--custom-switch-theme)

  (change-theme 'twilight-bright 'twilight-anti-bright))

(unless (featurep 'theme-changer)
  (load-theme 'twilight-anti-bright t))

(custom-set-faces
 '(cursor ((((class color) (background light)) :background "sky blue")))
 '(default ((((class color) (background light)) :foreground "#494949")
            (((class color) (background dark)) :foreground "#b8b8c1")))
 '(diff-added   ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22")))
 '(diff-changed ((((class color) (background light)) :background "#ffffdd" :foreground "#aaaa22")))
 '(diff-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222")))
 '(diff-refine-added   ((((class color) (background light)) :background "#eeffee" :foreground "#008800" :weight bold)))
 '(diff-refine-changed ((((class color) (background light)) :background "#ffffee" :foreground "#888800" :weight bold)))
 '(diff-refine-removed ((((class color) (background light)) :background "#ffeeee" :foreground "#880000" :weight bold)))
 '(fixed-pitch       ((t :family "DejaVu Sans Mono")))
 '(fixed-pitch-serif ((t :family "DejaVu Serif")))
 '(italic ((t :family "Fantasque Sans Mono" :height 110 :slant italic)))
 '(line-number
   ((((class color) (background light)) :inherit (fringe default) :foreground "grey80")
    (((class color) (background dark))  :inherit (fringe default) :foreground "grey40")))
 '(line-number-current-line ((((class color) (background light))
                              :inherit line-number :background "grey93"  :foreground "grey70" :weight semi-bold)
                             (((class color) (background dark))
                              :inherit line-number :background "#2b2f35" :foreground "grey50" :weight semi-bold)))
 '(link ((t :underline t)))
 '(shadow ((((class color) (background light)) :inherit default :foreground "grey60")
           (((class color) (background dark))  :inherit default :foreground "#606b78")))
 '(show-paren-match ((t :weight bold)))
 '(trailing-whitespace ((((class color) (background light)) :background "grey65")
                        (((class color) (background dark))  :background "grey35")))
 '(tooltip ((((class color) (background dark)) :background "#173735" :foreground "#dcdddd"))))

(use-package company
  :defer t
  :config
  (custom-set-faces
   '(company-scrollbar-bg ((((class color) (background dark)) :inherit company-tooltip)))
   `(company-scrollbar-fg ((((class color) (background dark)) :background "#a2561e")))
   `(company-tooltip ((((class color) (background dark)) :background "#deae3e" :foreground "#2a2921")))
   `(company-tooltip-selection ((((class color) (background dark)) :background "#d97a35")))
   `(company-tooltip-common ((((class color) (background dark)) :foreground "#b23f1e")))
   `(company-tooltip-annotation ((((class color) (background dark)) :foreground "#b23f1e")))
   '(company-preview ((t :inherit shadow :underline t)))
   '(company-preview-common ((t :inherit company-preview)))))

(use-package clojure-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face
       ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(clojure-cond-condtion-face ((t :inherit italic)))
     '(clojure-if-true-face ((t :inherit italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face)))
     '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit shadow)))
     '(clojure-meta-face ((t :inherit shadow :weight extra-light)))
     `(clojure-important-keywords-face
       ((((class color) (background light)) :inherit font-lock-keyword-face :foreground "#d67d00")
        (((class color) (background dark))  :inherit font-lock-keyword-face :foreground "#da7e3b")))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
     '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
     '(clojure-side-effect-face ((t :weight semi-bold :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face
       ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     '(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face :weight semi-bold))))))

(use-package cperl-mode
  :defer t
  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package eldoc
  :defer t
  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :weight semi-bold :underline t)))))

(use-package evil-goggles
  :defer t
  :config
  (let ((common-style
         '(:underline unspecified :slant unspecified :overline unspecified :box unspecified :inverse-video t)))
    (custom-set-faces
     `(evil-goggles-delete-face
       ((((class color) (background light)) :background "#eecccc" ,@common-style)
        (((class color) (background dark))  :inherit diff-removed ,@common-style)))
     `(evil-goggles-change-face
       ((((class color) (background light)) :background "#eecccc" ,@common-style)
        (((class color) (background dark))  :inherit diff-removed ,@common-style)))
     `(evil-goggles-paste-face
       ((((class color) (background light)) :background "#cceecc" ,@common-style)
        (((class color) (background dark))  :inherit diff-added   ,@common-style)))
     `(evil-goggles-yank-face
       ((((class color) (background light)) :background "#eeeecc" ,@common-style)
        (((class color) (background dark))  :inherit diff-changed ,@common-style)))
     '(evil-goggles-undo-redo-remove-face ((t :inherit evil-goggles-delete-face)))
     '(evil-goggles-undo-redo-add-face    ((t :inherit evil-goggles-paste-face)))
     '(evil-goggles-undo-redo-change-face ((t :inherit evil-goggles-change-face))))))

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused
     ((((class color) (background light)) :inherit font-lock-comment-face :background "#ffffff")
      (((class color) (background dark))  :inherit font-lock-comment-face :background "#1a1d1f")))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :foreground "#7f7b81" :slant unspecified :weight extra-light)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :weight extra-light)))
   '(font-lock-doc-face ((t :weight extra-light)))
   '(font-lock-function-name-face ((t :weight semi-bold)))
   '(font-lock-keyword-face ((((class color) (background light)) :background "#fbf5ec")))
   '(font-lock-preprocessor-face ((((class color) (background light)) :background "#fbf6ed")))
   '(font-lock-regexp-grouping-backslash
     ((((class color) (background light)) :inherit font-lock-string-face :background "#e1f2d6")
      (((class color) (background dark))  :inherit font-lock-string-face :background "#1e2826" :weight extra-light)))
   '(font-lock-regexp-grouping-construct
     ((((class color) (background light)) :inherit font-lock-string-face :background "#e1f2d6")
      (((class color) (background dark))  :inherit font-lock-string-face :background "#1e2826" :weight extra-light)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((((class color) (background light)) :foreground "#607596")))
   '(font-lock-warning-face ((t :weight semi-bold :underline t)))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   '(fringe ((((class color) (background light)) :background "grey99" :foreground "grey60")
             (((class color) (background dark))  :background "#161c22" :foreground "#7393b3")))))

(use-package hl-line
  :defer t
  :init
  (defface hl-line-evil-insert-light
    '((t :weight light))
    "TODO")
  (defface hl-line-evil-insert-normal
    '((t :weight normal))
    "TODO")
  (defface hl-line-evil-insert-semi-bold
    '((t :weight semi-bold))
    "TODO")
  (defface hl-line-evil-insert-bold
    '((t :weight bold))
    "TODO")
  (defface hl-line-evil-insert-extra-bold
    '((t :weight extra-bold))
    "TODO")

  :config
  (custom-set-faces
   '(hl-line ((t :background unspecified)))))

(use-package hl-todo
  :defer t
  :config
  (custom-set-faces
   '(hl-todo ((t :foreground "#cc9393" :weight semi-bold)))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit (highlight) :weight bold :underline t)))))

(use-package isearch
  :defer t
  :config
  (custom-set-faces
   '(isearch ((t :background "magenta3" :foreground "white" :weight bold)))
   '(lazy-highlight
     ((((class color) (background dark)) :background "paleturquoise4" :foreground "white" :weight bold)))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((((class color) (background light)) :background "#9fcdf2" :foreground "white" :weight bold)
                        (((class color) (background dark))  :background "#5f6369" :foreground "white" :weight bold)))
   '(ivy-grep-info ((((class color) (background dark)) :inherit font-lock-string-face)))
   '(ivy-minibuffer-match-face-1
     ((((class color) (background light)) :background "#d3d3d3" :foreground "#8a8a8a" :weight bold)
      (((class color) (background dark))  :background "#7d7e7f" :foreground "#535454" :weight bold)))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b" :weight bold)))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :foreground "#707099" :weight bold)))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f" :weight bold)))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

(use-package rpm-spec-mode
  :defer t
  :config
  (custom-set-faces
   '(rpm-spec-ghost-face ((t :inherit shadow)))
   '(rpm-spec-macro-face ((t :inherit font-lock-keyword-face)))
   '(rpm-spec-package-face ((t :inherit font-lock-constant-face)))
   '(rpm-spec-section-face ((t :inherit font-lock-function-name-face)))
   '(rpm-spec-tag-face ((t :inherit font-lock-builtin-face)))
   '(rpm-spec-var-face ((t :inherit font-lock-variable-name-face)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit highlight :underline t )))
   '(lsp-face-highlight-textual ((t :inherit highlight :underline t )))
   '(lsp-face-highlight-write   ((t :inherit highlight :underline t :weight semi-bold)))
   '(lsp-face-workspace-modeline ((t :weight extra-light)))))

(use-package magit
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     '(magit-diff-context-highlight ((t :background "#fbfeee" :foreground "#a4c207")))
     '(magit-diff-added   ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22")))
     '(magit-diff-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222")))
     '(magit-diff-added-highlight
       ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22" :weight semi-bold)))
     '(magit-diff-removed-highlight
       ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222" :weight semi-bold)))
     '(magit-diff-context ((t :foreground "grey50" :weight extra-light)))
     '(magit-diff-context-highlight ((t :inherit hl-line :foreground "grey50")))
     '(magit-diff-file-heading ((t :weight semi-bold)))
     `(magit-hash ((((class color) (background light)) :foreground "grey60" :weight ,default-weight)))
     `(magit-log-author ((((class color) (background light)) :foreground "firebrick" :weight ,default-weight)))
     `(magit-log-date ((((class color) (background light)) :foreground "grey30" :weight ,default-weight)))
     '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black"))))))

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc
     ((((class color) (background light)) :background "#fff6e5" :foreground "orange2")
      (((class color) (background dark))  :background "#201818" :foreground "tan1")))
   `(sh-quoted-exec
     ((((class color) (background light)) :background "#faecfa" :foreground "magenta")
      (((class color) (background light)) :background "#191719" :foreground "magenta")))))

(use-package swiper
  :defer t
  :config
  (custom-set-faces
   '(swiper-line-face
     ((((class color) (background light)) :background "#9fcdf2" :foreground "white" :weight bold)
      (((class color) (background dark))  :background "#5f6369" :foreground "white" :weight bold)))
   '(swiper-match-face-1
     ((((class color) (background light)) :inherit swiper-line-face :background "#d3d3d3" :weight bold)
      (((class color) (background dark))  :inherit swiper-line-face :background "#7d7e7f" :weight bold)))
   '(swiper-match-face-2 ((t :inherit swiper-line-face :background "#e99ce8" :weight bold)))
   '(swiper-match-face-3 ((t :inherit swiper-line-face :background "#bbbbff" :weight bold)))
   '(swiper-match-face-4 ((t :inherit swiper-line-face :background "#ffbbff" :weight bold)))
   '(swiper-background-match-face-1
     ((((class color) (background light)) :background "#d3d3d3" :foreground "#8a8a8a" :weight bold)
      (((class color) (background dark))  :background "#7d7e7f" :foreground "#535454" :weight bold)))
   '(swiper-background-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b" :weight bold)))
   '(swiper-background-match-face-3 ((t :background "#bbbbff" :foreground "#707099" :weight bold)))
   '(swiper-background-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f" :weight bold))))
  ;; copy from `swiper--recompute-background-faces'
  (let ((faces '(swiper-match-face-1
                 swiper-match-face-2
                 swiper-match-face-3
                 swiper-match-face-4))
        (colir-compose-method #'colir-compose-alpha)
        (line-bg (face-background 'swiper-line-face)))
    (cl-mapc (lambda (f1 f2)
               (let ((bg (face-background f1)))
                 (when bg
                   (set-face-background
                    f2
                    (colir-blend
                     (colir-color-parse bg)
                     (colir-color-parse line-bg))))))
             swiper-faces
             faces)))

(use-package treemacs
  :defer t
  :config
  (custom-set-faces
   `(treemacs-fringe-indicator-face ((t :foreground ,(face-background 'cursor))))
   '(treemacs-selected-icon
     ((((class color) (background light)) :background "#9fcdf2")
      (((class color) (background dark))  :background "#5f6369")))))

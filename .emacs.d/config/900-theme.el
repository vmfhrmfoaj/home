;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package twilight-bright-theme
  :ensure t
  :config
  (load-theme 'twilight-bright t)
  (custom-set-faces
   '(cursor ((t :background "sky blue")))
   '(diff-added   ((t :background "#ddffdd" :foreground "#22aa22")))
   '(diff-changed ((t :background "#ffffdd" :foreground "#aaaa22")))
   '(diff-removed ((t :background "#ffdddd" :foreground "#aa2222")))
   '(diff-refine-added   ((t :background "#eeffee" :foreground "#008800" :weight bold)))
   '(diff-refine-changed ((t :background "#ffffee" :foreground "#888800" :weight bold)))
   '(diff-refine-removed ((t :background "#ffeeee" :foreground "#880000" :weight bold)))
   '(line-number ((t :inherit (default) :background "grey99" :foreground "grey80")))
   '(line-number-current-line ((t :inherit (hl-line line-number) :background "#f1f8fd" :foreground "grey75")))
   '(fixed-pitch ((t :family "Dejavu Sans Mono")))
   '(link ((t :underline t)))
   '(shadow ((t :inherit default :foreground "grey55")))
   '(show-paren-match ((t :weight bold :underline t)))
   '(trailing-whitespace ((t :background "grey65")))))
(use-package twilight-anti-bright-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'twilight-anti-bright t)
  (custom-set-faces
   '(fixed-pitch ((t :family "Dejavu Sans Mono")))
   '(link ((t :underline t)))
   '(shadow ((t :inherit default :foreground "grey55")))
   '(show-paren-match ((t :weight bold :underline t)))
   '(trailing-whitespace ((t :background "grey35")))
   '(tooltip ((t :background "#173735" :foreground "#dcdddd")))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   '(cider-fringe-good-face ((t :inherit success)))
   '(clojure-define-type-face ((t :inherit font-lock-type-face)))
   '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
   `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
   '(clojure-important-keywords-face ((t :inherit font-lock-keyword-face) :weight semi-bold))
   '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   '(clojure-variable-name-face ((t :inherit (bold font-lock-variable-name-face))))
   '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   '(clojure-side-effect-face ((t :weight semi-bold :underline t)))
   '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))))

(use-package eldoc
  :defer t
  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :weight semi-bold :underline t)))))

(use-package evil-goggles
  :defer t
  :config
  (let ((common-style '(:underline unspecified :slant unspecified :overline unspecified :box unspecified :inverse-video t)))
    (custom-set-faces
     `(evil-goggles-delete-face ((t :inherit diff-refine-removed ,@common-style)))
     `(evil-goggles-change-face ((t :inherit diff-refine-removed ,@common-style)))
     `(evil-goggles-paste-face  ((t :inherit diff-refine-added   ,@common-style)))
     `(evil-goggles-yank-face   ((t :inherit diff-refine-changed ,@common-style)))
     '(evil-goggles-undo-redo-remove-face ((t :inherit evil-goggles-delete-face)))
     '(evil-goggles-undo-redo-add-face    ((t :inherit evil-goggles-paste-face)))
     '(evil-goggles-undo-redo-change-face ((t :inherit evil-goggles-change-face))))))

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused ((t :foreground "#a49da5")))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :background unspecified :slant unspecified :weight light)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :weight extra-light)))
   '(font-lock-function-name-face ((t :weight bold)))
   '(font-lock-regexp-grouping-backslash
     ((((class color) (background light)) :inherit font-lock-string-face :background "#e1f2d6")
      (((class color) (background dark))  :inherit font-lock-string-face :weight semi-bold)))
   '(font-lock-regexp-grouping-construct
     ((((class color) (background light)) :inherit font-lock-string-face :background "#e1f2d6")
      (((class color) (background dark))  :inherit font-lock-string-face :weight semi-bold)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((((class color) (background light)) :foreground "#607596")))
   '(font-lock-warning-face ((t :weight semi-bold :underline t)))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   '(fringe ((((class color) (background light)) :background "grey98")
             (((class color) (background dark)) :background "#171c22")))))

(use-package hl-line
  :defer t
  :init
  (defface hl-line-evil-insert
    '((((class color) (background light)) :background "#e2f0fb")
      (((class color) (background dark)) :weight bold))
    "TODO")

  :config
  (custom-set-faces
   '(hl-line ((((class color) (background light)) :background "#eef7fd")))))

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
   '(lazy-highlight ((((class color) (background dark)) :background "paleturquoise4" :foreground "white" :weight bold)))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((((class color) (background light)) :background "#9fcdf2" :foreground "white" :weight bold)
                        (((class color) (background dark))  :background "#d1d2d4" :foreground "black" :weight bold)))
   '(ivy-minibuffer-match-face-1 ((((class color) (background light)) :background "#d3d3d3" :foreground "#8a8a8a" :weight bold)
                                  (((class color) (background dark))  :background "#9d9d9d" :foreground "#6c6c6c" :weight bold)))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b" :weight bold)))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :foreground "#707099" :weight bold)))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f" :weight bold)))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit highlight :underline t )))
   '(lsp-face-highlight-textual ((t :inherit highlight :underline t )))
   '(lsp-face-highlight-write   ((t :inherit highlight :underline t :weight semi-bold)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((t :background "#fbfeee" :foreground "#a4c207")))
   '(magit-diff-added   ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22")))
   '(magit-diff-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222")))
   '(magit-diff-added-highlight   ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22" :weight semi-bold)))
   '(magit-diff-removed-highlight ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222" :weight semi-bold)))
   '(magit-diff-context ((t :foreground "grey50" :weight normal)))
   '(magit-diff-context-highlight ((t :inherit hl-line :foreground "grey50")))
   '(magit-diff-file-heading ((t :weight bold)))
   '(magit-hash ((((class color) (background light)) :foreground "grey60" :weight normal)))
   '(magit-log-author ((((class color) (background light)) :foreground "firebrick" :weight normal)))
   '(magit-log-date ((((class color) (background light)) :foreground "grey30" :weight normal)))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc
     ((((class color) (background light)) :background "#fcf7f2" :foreground "tan1")
      ((((class color) (background dark)) :background "#201818" :foreground "tan1"))))
   `(sh-quoted-exec
     ((((class color) (background light)) :background "#faecfa" :foreground "magenta")
      (((class color) (background light)) :background "#191719" :foreground "magenta")))))

(use-package swiper
  :defer t
  :config
  (custom-set-faces
   '(swiper-line-face
     ((((class color) (background light)) :background "#9fcdf2" :foreground "white" :weight bold)
      (((class color) (background dark))  :background "#d1d2d4" :foreground "black" :weight bold)))
   '(swiper-match-face-1
     ((((class color) (background light)) :inherit swiper-line-face :background "#d3d3d3" :weight bold)
      (((class color) (background dark))  :inherit swiper-line-face :background "#9d9d9d" :weight bold)))
   '(swiper-match-face-2 ((t :inherit swiper-line-face :background "#e99ce8" :weight bold)))
   '(swiper-match-face-3 ((t :inherit swiper-line-face :background "#bbbbff" :weight bold)))
   '(swiper-match-face-4 ((t :inherit swiper-line-face :background "#ffbbff" :weight bold)))
   '(swiper-background-match-face-1
     ((((class color) (background light)) :inherit swiper-match-face-1 :foreground "#8a8a8a")
      (((class color) (background dark))  :inherit swiper-match-face-1 :foreground "#6c6c6c")))
   '(swiper-background-match-face-2 ((t :inherit swiper-match-face-2 :foreground "#8b5d8b")))
   '(swiper-background-match-face-3 ((t :inherit swiper-match-face-3 :foreground "#707099")))
   '(swiper-background-match-face-4 ((t :inherit swiper-match-face-4 :foreground "#7f5d7f")))))

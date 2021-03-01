;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package base16-theme
  :ensure t
  :config
  (setq frame-background-mode 'dark)
  (load-theme 'base16-tomorrow-night t)

  (custom-set-faces
   '(highlight ((t :background "#484a4e" :foreground "#d5d8d6" :weight bold)))
   '(italic ((t :family "Fantasque Sans Mono" :height 110 :slant italic)))
   '(line-number ((t :background "#222326" :weight extra-light)))
   '(line-number-current-line
     ((t :inherit line-number :background "#484a4e" :foreground "#bfc2c0" :weight light :inverse-video nil)))
   '(mode-line-inactive ((t :weight light)))
   '(mode-line ((t :weight normal)))
   '(region ((t :background "#760e17")))
   '(shadow ((t :weight normal)))
   '(show-paren-match ((t :background unspecified :foreground "firebrick1" :underline t)))
   '(show-paren-mismatch ((t :background "red3" :foreground "#f0d2cd" :weight bold :underline t)))
   '(whitespace-newline ((t :background unspecified :weight extra-light)))
   '(whitespace-space   ((t :background unspecified :weight extra-light)))
   '(whitespace-tab     ((t :background unspecified :weight extra-light)))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((((background dark))
       :foreground ,(color-from 'default :foreground -5)
       :background ,(color-from 'default :background +3))))))

(use-package company
  :defer t
  :config
  (custom-set-faces
   '(company-preview ((t :inherit shadow :underline t)))
   '(company-preview-common ((t :inherit company-preview)))
   '(company-scrollbar-bg ((t :background "#242529")))
   '(company-scrollbar-fg ((t :background "#5a5b5a")))))

(use-package clojure-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face
       ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(clojure-cond-condtion-face ((t :inherit italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face)))
     '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit shadow)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face
                                                    :foreground "#a35151"
                                                    :weight ,default-weight)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face :weight bold)))
     '(clojure-side-effect-face ((t :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face
       ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     '(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face))))))

(use-package cperl-mode
  :defer t
  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package diff-mode
  :defer t
  :config
  (custom-set-faces
   '(diff-refine-added   ((t :background "#22aa22" :weight bold)))
   '(diff-refine-changed ((t :background "#aaaa22" :weight bold)))
   '(diff-refine-removed ((t :background "#aa2222" :weight bold)))))

(use-package eldoc
  :defer t
  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :weight bold :underline t)))))

(use-package elisp-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face
                                                 :foreground "#a35151"
                                                 :weight ,default-weight))))))

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
  (custom-set-faces `(focus-unfocused ((t :foreground "#606060")))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :weight light)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :foreground "#787978")))
   '(font-lock-doc-face ((t :weight normal)))
   '(font-lock-function-name-face ((t :foreground "#85aacc" :weight bold)))
   '(font-lock-negation-char-face ((t :inherit font-lock-warning-face :foreground unspecified)))
   '(font-lock-regexp-grouping-backslash ((t :weight semi-bold)))
   '(font-lock-regexp-grouping-construct ((t :weight semi-bold)))
   '(font-lock-string-face ((t :weight normal)))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   '(fringe ((t :foreground "#969896")))))

(use-package go-mode
  :defer t
  :config
  (custom-set-faces
   '(golang-type-definition-face ((t :inherit font-lock-type-face :weight bold)))
   '(golang-interface-method-face ((t :inherit font-lock-function-name-face :weight semi-bold)))))

(use-package hl-todo
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(hl-todo ((t :foreground "#cc9393" :weight ,default-weight))))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((t :background "#5f6369" :foreground "white" :weight bold)))
   '(ivy-grep-info ((t :inherit font-lock-string-face :weight unspecified)))
   '(ivy-minibuffer-match-face-1 ((t :background "#7d7e7f" :foreground "#535454" :weight bold)))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b" :weight bold)))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :foreground "#707099" :weight bold)))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f" :weight bold)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-textual ((t :underline t)))
   '(lsp-face-highlight-read    ((t :underline t)))
   '(lsp-face-highlight-write   ((t :underline t :wegith bold)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-added-highlight   ((t :background "#336633" :foreground "#ddffdd" :weight bold)))
   '(magit-diff-removed-highlight ((t :background "#663333" :foreground "#ffdddd" :weight bold)))
   `(magit-diff-context
     ((t :inherit magit-diff-context-highlight :background ,(bg-color-from 'default) :weight light)))
   `(magit-diff-context-highlight ((t :weight normal)))
   '(magit-diff-file-heading ((t :weight bold)))))

(use-package markdown-mode
  :defer t
  :config
  (custom-set-faces
   '(markdown-markup-face ((t :inherit shadow :slant normal :weight light)))
   '(markdown-header-delimiter-face ((t :inherit markdown-markup-face :weight extra-light)))))

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
   '(rpm-spec-changelog-item-face ((t :weight normal)))
   '(rpm-spec-ghost-face ((t :inherit shadow)))
   '(rpm-spec-macro-face ((t :inherit font-lock-keyword-face)))
   '(rpm-spec-package-face ((t :inherit font-lock-constant-face)))
   '(rpm-spec-section-face ((t :inherit font-lock-function-name-face)))
   '(rpm-spec-tag-face ((t :inherit font-lock-builtin-face)))
   '(rpm-spec-var-face ((t :inherit font-lock-variable-name-face)))))

(use-package rust-mode
  :defer t
  :config
  (custom-set-faces
   '(rust-attribute-face ((t :inherit font-lock-preprocessor-face :weight normal)))
   '(rust-lifetime-face ((t :inherit font-lock-variable-name-face :weight normal)))
   `(rust-string-interpolation-face
     ((t :inherit (font-lock-regexp-grouping-construct font-lock-string-face) :slant unspecified)))))

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
   '(swiper-line-face ((t :background "#5f6369" :foreground "white" :weight bold :extend t)))
   '(swiper-match-face-1 ((t :background "#7d7e7f" :weight bold)))
   '(swiper-match-face-2 ((t :background "#e99ce8" :weight bold)))
   '(swiper-match-face-3 ((t :background "#bbbbff" :weight bold)))
   '(swiper-match-face-4 ((t :background "#ffbbff" :weight bold)))
   '(swiper-background-match-face-1 ((t :background "#7d7e7f" :foreground "#535454" :weight bold)))
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
   `(treemacs-fringe-indicator-face ((t :foreground ,(face-background 'cursor))))))

(use-package web-mode
  :defer t
  :config
  (custom-set-faces
   '(web-mode-html-attr-equal-face ((t :inherit shadow)))))

(use-package yascroll
  :defer t
  :config
  (custom-set-faces
   '(yascroll:thumb-fringe ((t :background "#969896")))))

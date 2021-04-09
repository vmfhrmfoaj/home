;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package base16-theme
  :ensure t
  :init
  (eval-when-compile (require 'base16-theme nil t))

  :config
  (setq frame-background-mode 'dark)
  (load-theme 'base16-tomorrow-night t)

  (custom-set-faces
   '(fixed-pitch ((t :inherit default)))
   '(highlight ((t :background "#484a4e" :foreground "#d5d8d6")))
   '(isearch ((t :weight bold)))
   '(italic ((t :family "Fantasque Sans Mono" :height 110 :slant italic)))
   '(lazy-highlight ((t :weight bold)))
   '(line-number ((t :background "#222326" :foreground "#717371" :weight light :extend t)))
   '(line-number-current-line
     ((t :inherit line-number :background "#484a4e" :foreground "#bfc2c0" :weight light :inverse-video nil :extend t)))
   '(mode-line-inactive ((t :foreground "#717371" :weight normal)))
   '(region ((t :background "#760e17")))
   '(shadow ((t :weight light)))
   '(show-paren-match ((t :background unspecified :foreground "red" :weight extra-bold :box (:line-width (-1 . -1)))))
   '(show-paren-mismatch ((t :background "red3" :foreground "#f0d2cd" :weight extra-bold :underline t)))
   '(symbol-dash-or-underline-face ((t :weight light)))
   '(whitespace-newline ((t :background unspecified :foreground "#646664" :weight light)))
   '(whitespace-space   ((t :background unspecified :foreground "#646664" :weight light)))
   '(whitespace-tab     ((t :background unspecified :foreground "#646664" :weight light)))
   '(vertical-border ((t :foreground "#222326")))))

(use-package auto-dim-other-buffers
  :defer t
  :init
  (eval-when-compile (require 'auto-dim-other-buffers nil t))

  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((t :background ,(color-from 'default :background 4)
         :foreground ,(color-from 'default :foreground -15))))
   `(auto-dim-other-line-number-face
     ((t :background ,(color-from 'default :background 3.5)
         :foreground ,(color-from 'line-number :foreground -5))))))

(use-package company
  :defer t
  :init
  (eval-when-compile (require 'company nil t))

  :config
  (custom-set-faces
   '(company-preview ((t :inherit shadow :underline t)))
   '(company-preview-common ((t :inherit company-preview)))
   '(company-scrollbar-bg ((t :background "#242529")))
   '(company-scrollbar-fg ((t :background "#5a5b5a")))))

(use-package clojure-mode
  :defer t
  :init
  (eval-when-compile (require 'clojure-mode nil t))

  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face
       ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(cider-repl-stdout-face ((t :inherit font-lock-string-face :weight light)))
     '(clojure-cond-condtion-face ((t :inherit italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face)))
     '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit clojure-fn-parameter-face :weight normal)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face :foreground unspecified)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :foreground "#b36262" :weight ,default-weight)))
     '(clojure-local-binding-variable-name-unsed-face ((t :inherit clojure-local-binding-variable-name-face :weight normal)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face)))
     '(clojure-side-effect-face ((t :inherit italic :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face
       ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     `(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face
                                            :weight ,(face-attribute 'font-lock-function-name-face :weight)))))))

(use-package cperl-mode
  :defer t
  :init
  (eval-when-compile (require 'cperl-mode nil t))

  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package counsel
  :defer t
  :init
  (eval-when-compile (require 'counsel nil t))

  :config
  (custom-set-faces
   '(counsel-company-annotation-face ((t :inherit shadow :height 0.9)))))

(use-package diff-mode
  :defer t
  :init
  (eval-when-compile (require 'diff-mode nil t))

  :config
  (custom-set-faces
   '(diff-refine-added   ((t :background "#339933")))
   '(diff-refine-changed ((t :background "#999933")))
   '(diff-refine-removed ((t :background "#aa3333")))))

(use-package eldoc
  :defer t
  :init
  (eval-when-compile (require 'eldoc nil t))

  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :underline t)))))

(use-package elisp-mode
  :defer t
  :init
  (eval-when-compile (require 'elisp-mode nil t))

  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :foreground "#b36262" :weight ,default-weight))))))

(use-package eshell
  :defer t
  :init
  (eval-when-compile (require 'eshell nil t))

  :config
  (custom-set-faces
   '(eshell-prompt ((t :inherit shadow)))))

(use-package evil-goggles
  :defer t
  :init
  (eval-when-compile (require 'evil-goggles nil t))

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
  :init
  (eval-when-compile (require 'focus nil t))

  :config
  (custom-set-faces `(focus-unfocused ((t :foreground "#606060")))))

(use-package font-lock
  :defer t
  :init
  (eval-when-compile (require 'font-lock nil t))

  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :foreground "#717371" :weight light)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :foreground unspecified)))
   '(font-lock-doc-face ((t :weight normal)))
   '(font-lock-function-name-face ((t :foreground "#85aacc" :weight ultra-bold)))
   '(font-lock-negation-char-face ((t :inherit font-lock-warning-face :foreground unspecified)))
   '(font-lock-regexp-grouping-backslash ((t :weight normal)))
   '(font-lock-regexp-grouping-construct ((t :weight normal)))))

(use-package fringe
  :defer t
  :init
  (eval-when-compile (require 'fringe nil t))

  :config
  (custom-set-faces
   '(fringe ((t :foreground "#454645")))))

(use-package go-mode
  :defer t
  :init
  (eval-when-compile (require 'go-mode nil t))

  :config
  (custom-set-faces
   '(golang-type-definition-face ((t :inherit font-lock-type-face)))
   '(golang-interface-method-face ((t :inherit font-lock-function-name-face)))))

(use-package hl-todo
  :defer t
  :init
  (eval-when-compile (require 'hl-todo nil t))

  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(hl-todo ((t :foreground "#cc9393" :weight ,default-weight))))))

(use-package highlight-parentheses
  :defer t
  :init
  (eval-when-compile (require 'highlight-parentheses nil t))

  :config
  (custom-set-faces
   '(highlight-parentheses-highlight ((t :weight normal :underline t)))))

(use-package highlight-numbers
  :defer t
  :init
  (eval-when-compile (require 'highlight-numbers nil t))

  :config
  (custom-set-faces
   '(highlight-numbers-number ((t :inherit font-lock-constant-face)))))

(use-package ivy
  :defer t
  :init
  (eval-when-compile (require 'ivy nil t))

  :config
  (custom-set-faces
   `(ivy-current-match ((t :background ,(color-from 'default :background)
                           :foreground "white" :weight extra-bold
                           :box (:line-width (-1 . -1) :color "#717371"))))
   '(ivy-grep-info ((t :inherit font-lock-string-face :weight unspecified)))
   '(ivy-minibuffer-match-face-1 ((t :background "#7d7e7f" :foreground "#535454" :weight normal)))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b" :weight normal)))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :foreground "#707099" :weight normal)))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f" :weight normal)))))

(use-package lsp-mode
  :defer t
  :init
  (eval-when-compile (require 'lsp-mode nil t))

  :config
  (custom-set-faces
   '(lsp-details-face ((t :inherit shadow :height 0.9)))
   '(lsp-face-highlight-textual ((t :underline t)))
   '(lsp-face-highlight-read    ((t :underline t)))
   '(lsp-face-highlight-write   ((t :underline t)))
   '(lsp-punctuation-face ((t :inherit unspecified)))))

(use-package magit
  :defer t
  :init
  (eval-when-compile (require 'magit nil t))

  :config
  (custom-set-faces
   '(magit-diff-added             ((t :background "#224422" :foreground "#cceecc")))
   '(magit-diff-added-highlight   ((t :background "#336633" :foreground "#ddffdd")))
   '(magit-diff-removed           ((t :background "#442222" :foreground "#eecccc")))
   '(magit-diff-removed-highlight ((t :background "#663333" :foreground "#ffdddd")))
   `(magit-diff-context
     ((t :inherit magit-diff-context-highlight :background ,(bg-color-from 'default) :foreground "#717371" :weight normal)))
   `(magit-diff-context-highlight ((t :weight normal)))))

(use-package markdown-mode
  :defer t
  :init
  (eval-when-compile (require 'markdown-mode nil t))

  :config
  (custom-set-faces
   '(markdown-markup-face ((t :foreground "#717371" :slant normal :weight normal)))
   '(markdown-header-delimiter-face ((t :inherit markdown-markup-face)))))

(use-package org
  :defer t
  :init
  (eval-when-compile (require 'org nil t))

  :config
  (custom-set-faces
   '(org-agenda-date ((t :foreground "dark cyan" :height 1.1)))
   '(org-agenda-date-weekend ((t :inherit org-agenda-date)))
   '(org-agenda-date-today ((t :inherit org-agenda-date :foreground "turquoise")))
   '(org-agenda-calendar-event ((t :weight normal)))
   '(org-block ((t :foreground unspecified :weight normal)))
   '(org-checkbox-statistics-done ((t :foreground "#b5bd68" :weight normal)))
   '(org-checkbox-statistics-todo ((t :foreground "#cc6666" :weight normal)))
   '(org-date ((t :underline unspecified :weight normal)))
   '(org-done ((t  :weight normal :inverse-video t)))
   '(org-drawer ((t :foreground "light sky blue" :weight normal)))
   '(org-headline-done ((t :foreground "#a6a8a6" :weight normal)))
   `(org-hide ((t :inherit default :background unspecified :foreground ,(bg-color-from 'default))))
   '(org-link ((t :inherit link :underline unspecified)))
   '(org-meta-line ((t :inherit font-lock-comment-face)))
   '(org-parenthesis-context-face ((t :inherit default :weight normal)))
   '(org-property-value ((t :weight normal)))
   '(org-special-keyword ((t :weight normal)))
   '(org-tag ((t :weight normal)))
   '(org-task-done ((t :inherit org-headline-done :strike-through t)))
   '(org-todo ((t :weight normal :inverse-video t)))
   '(org-warning ((t :inherit font-lock-warning-face :underline nil)))))

(use-package php-mode
  :defer t
  :init
  (eval-when-compile (require 'php-mode nil t))

  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

(use-package powerline
  :defer t
  :init
  (eval-when-compile (require 'powerline nil t))

  :config
  (custom-set-faces
   '(powerline-inactive2 ((t :background "#282a2e")))))

(use-package rpm-spec-mode
  :defer t
  :init
  (eval-when-compile (require 'rpm-spec-mode nil t))

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
  :init
  (eval-when-compile (require 'rust-mode nil t))

  :config
  (custom-set-faces
   '(rust-attribute-face ((t :inherit font-lock-preprocessor-face :weight normal)))
   '(rust-lifetime-face ((t :inherit font-lock-variable-name-face :weight normal)))
   `(rust-string-interpolation-face
     ((t :inherit (font-lock-regexp-grouping-construct font-lock-string-face) :slant unspecified)))))

(use-package sh-script
  :defer t
  :init
  (eval-when-compile (require 'sh-script nil t))

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
  :init
  (eval-when-compile (require 'swiper nil t))

  :config
  (custom-set-faces
   '(swiper-line-face ((t :background "#5f6369" :foreground "white" :weight bold :extend t)))
   '(swiper-match-face-1 ((t :background "#7d7e7f")))
   '(swiper-match-face-2 ((t :background "#e99ce8")))
   '(swiper-match-face-3 ((t :background "#bbbbff")))
   '(swiper-match-face-4 ((t :background "#ffbbff")))
   '(swiper-background-match-face-1 ((t :background "#7d7e7f" :foreground "#535454")))
   '(swiper-background-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b")))
   '(swiper-background-match-face-3 ((t :background "#bbbbff" :foreground "#707099")))
   '(swiper-background-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f"))))
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
  :init
  (eval-when-compile (require 'treemacs nil t))

  :config
  (custom-set-faces
   `(treemacs-fringe-indicator-face ((t :foreground ,(face-background 'cursor))))))

(use-package web-mode
  :defer t
  :init
  (eval-when-compile (require 'web-mode nil t))

  :config
  (custom-set-faces
   '(web-mode-html-attr-equal-face ((t :inherit shadow)))))

(use-package yascroll
  :defer t
  :init
  (eval-when-compile (require 'yascroll nil t))

  :config
  (let ((fringe-fg (color-from 'fringe :foreground)))
    (custom-set-faces
     `(yascroll:thumb-fringe ((t :background ,fringe-fg :foreground ,fringe-fg))))))

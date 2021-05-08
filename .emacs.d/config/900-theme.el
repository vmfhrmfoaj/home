;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package leuven-theme
  :ensure t
  :init
  (eval-when-compile (require 'leuven-theme nil t))

  :config
  (setq frame-background-mode 'light)
  (load-theme 'leuven t)

  (custom-set-faces
   '(bold ((t :foreground unspecified)))
   `(isearch ((t :background "#ffff00" :foreground "black" :underline unspecified :box (:line-width (-1 . -1) :color "#e5e500"))))
   '(italic ((t :foreground unspecified :family "Fantasque Sans Mono" :height 110 :slant italic)))
   `(lazy-highlight ((t :background "#eeee00" :foreground "grey10" :box (:line-width (-1 . -1) :color "#d4d400"))))
   `(line-number ((t :background ,(color-from 'default :background -2)
                     :foreground ,(color-from 'default :foreground 25)
                     :weight light)))
   '(line-number-current-line ((t :inherit line-number)))
   '(mode-line ((t :box (:line-width (-1 . -1) :color "#1a2f54"))))
   '(mode-line-inactive ((t :weight light :box unspecified)))
   '(shadow ((t :weight normal)))
   '(show-paren-match ((t :background unspecified :foreground "#0081b8" :underline t :box (:line-width (-1 . -1)))))
   '(show-paren-mismatch ((t :weight bold :underline t)))
   '(symbol-dash-or-underline-face ((t :weight light)))
   '(whitespace-newline ((t :weight light)))
   '(whitespace-space   ((t :weight light)))
   '(whitespace-tab     ((t :weight light)))
   '(vertical-border ((t :foreground "#cccccc")))))

(use-package clojure-mode
  :defer t
  :init
  (eval-when-compile (require 'clojure-mode nil t))

  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face  ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(cider-repl-stdout-face ((t :inherit font-lock-string-face :weight light)))
     '(clojure-cond-condtion-face ((t :inherit italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face)))
     '(clojure-defining-spec-face ((t :inherit font-lock-function-name-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit clojure-fn-parameter-face :weight normal)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face :foreground unspecified)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-local-binding-variable-name-unsed-face ((t :inherit clojure-local-binding-variable-name-face :weight normal)))
     '(clojure-punctuation-face ((t :inherit shadow :weight light)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face)))
     '(clojure-side-effect-face ((t :inherit italic :weight bold :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     `(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face
                                            :weight ,(face-attribute 'font-lock-function-name-face :weight)))))))

(use-package company
  :defer t
  :init
  (eval-when-compile (require 'company nil t))

  :config
  (custom-set-faces
   `(completions-common-part ((t :foreground ,(color-from 'default :foreground) :weight normal)))))

(use-package counsel
  :defer t
  :init
  (eval-when-compile (require 'counsel nil t))

  :config
  (custom-set-faces
   '(counsel-company-annotation-face ((t :inherit shadow :height 0.9)))))

(use-package cperl-mode
  :defer t
  :init
  (eval-when-compile (require 'cperl-mode nil t))

  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package diff-mode
  :defer t
  :init
  (eval-when-compile (require 'diff-mode nil t))

  :config
  (custom-set-faces
   '(diff-refine-added   ((t :weight bold)))
   '(diff-refine-changed ((t :weight bold)))
   '(diff-refine-removed ((t :weight bold)))))

(use-package elisp-mode
  :defer t
  :init
  (eval-when-compile (require 'elisp-mode nil t))

  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(lisp-punctuation-face ((t :inherit shadow :weight light))))))

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
  (let ((common-style '(:underline unspecified :slant unspecified :overline unspecified :box unspecified :inverse-video nil)))
    (custom-set-faces
     `(evil-goggles-delete-face
       ((t :background "#eecccc" ,@common-style)))
     `(evil-goggles-change-face
       ((t :background "#eecccc" ,@common-style)))
     `(evil-goggles-paste-face
       ((t :background "#cceecc" ,@common-style)))
     `(evil-goggles-yank-face
       ((t :background "#eeeecc" ,@common-style))))))

(use-package focus
  :defer t
  :init
  (eval-when-compile (require 'focus nil t))

  :config
  (custom-set-faces
   '(focus-unfocused ((t :foreground "#adadad")))))

(use-package font-lock
  :defer t
  :init
  (eval-when-compile (require 'font-lock nil t))

  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :family "Fantasque Sans Mono" :height 110 :weight light :slant unspecified)))
   '(font-lock-doc-face ((t :family "Fantasque Sans Mono" :height 110 :weight light)))
   '(font-lock-function-name-face ((t :weight bold)))
   '(font-lock-keyword-face ((t :weight unspecified)))
   '(font-lock-negation-char-face ((t :inherit font-lock-warning-face :foreground unspecified)))
   '(font-lock-regexp-grouping-backslash ((t :weight bold)))
   '(font-lock-regexp-grouping-construct ((t :weight bold)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((t :weight unspecified))))
  (custom-set-faces
   `(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face
                                          :foreground ,(color-from 'font-lock-comment-face :foreground 10))))))

(use-package fringe
  :defer t
  :init
  (eval-when-compile (require 'fringe nil t))

  :config
  (custom-set-faces
   `(fringe ((t :background ,(color-from 'default :background -3) :foreground "#a5ceec")))))

(use-package go-mode
  :defer t
  :init
  (eval-when-compile (require 'go-mode nil t))

  :config
  (custom-set-faces
   '(golang-type-definition-face ((t :inherit font-lock-type-face)))
   '(golang-interface-method-face ((t :inherit font-lock-function-name-face)))))

(use-package highlight-parentheses
  :defer t
  :init
  (eval-when-compile (require 'highlight-parentheses nil t))

  :config
  (setq highlight-parentheses-colors
        (->> (--iterate (dim-color it 5) (color-from 'cursor :background) 5)
             (-drop 1)
             (reverse)))

  (custom-set-faces
   '(highlight-parentheses-highlight ((t :weight normal :underline t)))))

(use-package ivy
  :defer t
  :init
  (eval-when-compile (require 'ivy nil t))

  :config
  (custom-set-faces
   '(ivy-current-match ((t :background "#f6fecd" :foreground "black" :weight bold :box (:line-width (-1 . -1) :color "#d0372d"))))
   '(ivy-grep-info ((t :inherit font-lock-string-face :weight unspecified)))))

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
   '(lsp-modeline-code-actions-face ((t :foreground unspecified)))))

(use-package magit
  :defer t
  :init
  (eval-when-compile (require 'magit nil t))

  :config
  (custom-set-faces
   '(magit-branch-current ((t :inherit magit-branch-local :box (:line-width (1 . -1)))))
   '(magit-branch-remote-head ((t :inherit magit-branch-remote :box (:line-width (1 . -1)))))
   '(magit-diff-context ((t :weight light)))
   '(magit-diff-context-highlight ((t :weight normal)))
   '(magit-section-highlight ((t :box unspecified)))))

(use-package org
  :defer t
  :init
  (eval-when-compile (require 'org nil t))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (add-to-list 'face-remapping-alist
                           `(org-tag (:inherit default :foreground ,(color-from 'org-tag :foreground) :weight normal)))))

  :config
  (custom-set-faces
   `(org-block-begin-line ((t :foreground ,(color-from 'org-block-begin-line :background -25) :weight normal :underline unspecified :extend t)))
   `(org-block-end-line ((t :foreground ,(color-from 'org-block-end-line :background -25) :weight normal :overline unspecified :extend t)))
   '(org-done ((t :box (:line-width (1 . -1)))))
   '(org-drawer ((t :weight normal)))
   '(org-headline-done ((t :weight unspecified :height unspecified)))
   '(org-tag ((t :background unspecified :slant unspecified)))
   '(org-parenthesis-context-face ((t :inherit default :weight normal)))
   '(org-special-keyword ((t :weight normal)))
   '(org-todo ((t :box (:line-width (1 . -1)))))))

(use-package php-mode
  :defer t
  :init
  (eval-when-compile (require 'php-mode nil t))

  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

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

(use-package powerline
  :defer t
  :init
  (eval-when-compile (require 'powerline nil t))

  :config
  (custom-set-faces
   '(powerline-active1 ((t :background "#edf8fc" :foreground "#335ea8")))))

(use-package spaceline
  :defer t
  :init
  (eval-when-compile (require 'spaceline nil t))

  :config
  (custom-set-faces
   `(spaceline-symbol-segment-face ((t :inherit powerline-active1 :weight normal)))))

(use-package web-mode
  :defer t
  :init
  (eval-when-compile (require 'web-mode nil t))

  :config
  (custom-set-faces
   '(web-mode-html-attr-equal-face ((t :inherit shadow)))))

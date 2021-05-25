;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package leuven-theme
  :ensure t
  :config
  (setq frame-background-mode 'light)
  (load-theme 'leuven t)

  (custom-set-faces
   '(c-style-brace-face ((t :inherit shadow :weight light)))
   '(c-style-operator-face ((t :inherit default :weight normal)))
   '(c-style-punctuation-1-face ((t :inherit shadow)))
   '(c-style-punctuation-2-face ((t :inherit shadow :weight light)))
   '(bold ((t :foreground unspecified)))
   `(isearch ((t :background "#ffff00" :foreground "black" :underline unspecified :box (:line-width (-1 . -1) :color "#e5e500"))))
   '(italic ((t :foreground unspecified :family "Fantasque Sans Mono" :slant italic)))
   `(lazy-highlight ((t :background "#eeee00" :foreground "grey10" :box (:line-width (-1 . -1) :color "#d4d400"))))
   `(line-number ((t :background ,(color-from 'default :background -2)
                     :foreground ,(color-from 'default :foreground 25)
                     :weight light)))
   '(line-number-current-line ((t :inherit line-number)))
   '(mode-line ((t :box (:line-width (-1 . -1) :color "#1a2f54"))))
   '(mode-line-inactive ((t :weight light :box unspecified)))
   '(shadow ((t :weight normal)))
   '(show-paren-match ((t :background unspecified :foreground "#0081b8" :weight bold :underline t :box (:line-width (-1 . -1)))))
   '(show-paren-mismatch ((t :weight bold :underline t)))
   '(whitespace-newline ((t :inherit whitespace-tab)))
   '(whitespace-space   ((t :inherit whitespace-tab)))
   '(whitespace-tab     ((t :foreground "#cccccc" :weight light)))
   '(vertical-border ((t :foreground "#cccccc")))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   '(auto-dim-other-buffers-face ((t :foreground "#474747")))
   '(auto-dim-other-line-number-face ((t :inherit line-number :foreground "#c6c6c6")))))

(use-package cider-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face  ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:style wave :color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(cider-repl-prompt-face ((t :inherit font-lock-keyword-face :weight light)))
     '(cider-repl-stdout-face ((t :inherit font-lock-string-face :weight light))))))

(use-package clojure-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     '(clojure-cond-condtion-face ((t :underline (:color "#adadad"))))
     '(clojure-define-type-face ((t :inherit font-lock-type-face :background "#efeaf5")))
     '(clojure-defining-spec-face ((t :inherit font-lock-function-name-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit clojure-fn-parameter-face :weight normal)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face :foreground unspecified)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     '(clojure-ns-definition-face ((t :inherit font-lock-type-face :background "#efeaf5")))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-local-binding-variable-name-unsed-face ((t :inherit clojure-local-binding-variable-name-face :weight normal)))
     '(clojure-punctuation-face ((t :inherit shadow :weight light)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face)))
     '(clojure-side-effect-face ((t :weight bold :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     `(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face
                                            :background "#f8eaf6"
                                            :weight ,(face-attribute 'font-lock-function-name-face :weight)))))))

(use-package company
  :defer t
  :config
  (custom-set-faces
   '(company-tooltip ((t :background "#eeeeee")))
   '(company-tooltip-annotation-selection ((t :foreground "#cccccc" :weight normal)))
   '(company-tooltip-common ((t :background "#e99ce8" :foreground "black" :weight unspecified)))
   '(company-tooltip-common-selection ((t :foreground "#e99ce8" :weight bold)))
   '(company-tooltip-selection ((t :foreground "white" :background "black" :weight bold)))
   `(completions-common-part ((t :foreground ,(color-from 'default :foreground) :weight normal)))))

(use-package counsel
  :defer t
  :config
  (custom-set-faces
   '(counsel-company-annotation-face ((t :inherit shadow :height 0.9)))))

(use-package cperl-mode
  :defer t
  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package diff-mode
  :defer t
  :config
  (custom-set-faces
   '(diff-refine-added   ((t :weight bold)))
   '(diff-refine-changed ((t :weight bold)))
   '(diff-refine-removed ((t :weight bold)))))

(use-package elisp-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(lisp-punctuation-face ((t :inherit shadow :weight light))))))

(use-package eshell
  :defer t
  :config
  (custom-set-faces
   '(eshell-prompt ((t :inherit shadow :weight light)))))

(use-package evil-goggles
  :defer t
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

(use-package flycheck
  :defer t
  :config
  (custom-set-faces
   '(flycheck-error   ((t :weight unspecified)))
   '(flycheck-warning ((t :weight unspecified)))
   '(flycheck-info    ((t :weight unspecified)))))

(use-package focus
  :defer t
  :config
  (custom-set-faces
   '(focus-unfocused ((t :foreground "#adadad")))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :inherit italic :weight light)))
   '(font-lock-doc-face ((t :inherit italic :weight light)))
   '(font-lock-function-name-face ((t :background "#e8f1f5" :foreground "#1975a3" :weight bold)))
   '(font-lock-keyword-face ((t :weight unspecified)))
   '(font-lock-negation-char-face ((t :inherit font-lock-warning-face :foreground unspecified)))
   '(font-lock-preprocessor-face ((t :weight normal)))
   '(font-lock-regexp-grouping-backslash ((t :weight normal)))
   '(font-lock-regexp-grouping-construct ((t :weight normal)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((t :weight unspecified))))
  (custom-set-faces
   `(font-lock-comment-delimiter-face ((t :foreground ,(color-from 'font-lock-comment-face :foreground 10) :weight light)))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   `(fringe ((t :background ,(color-from 'default :background -3) :foreground "#a5ceec")))))

(use-package go-mode
  :defer t
  :config
  (custom-set-faces
   '(golang-type-definition-face ((t :inherit font-lock-type-face)))
   '(golang-interface-method-face ((t :inherit font-lock-function-name-face)))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq highlight-parentheses-colors
        (->> (--iterate (dim-color it 5) (color-from 'cursor :background) 5)
             (-drop 1)
             (reverse)))

  (custom-set-faces
   '(highlight-parentheses-highlight ((t :weight bold :underline t)))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((t :background "black" :foreground "white" :weight bold)))
   '(ivy-grep-info ((t :inherit font-lock-string-face :weight light)))
   '(ivy-grep-line-number ((t :inherit compilation-line-number :weight light)))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :weight normal)))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :weight normal)))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :weight normal)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-details-face ((t :inherit shadow :height 0.9)))
   '(lsp-face-highlight-textual ((t :underline t)))
   '(lsp-face-highlight-read    ((t :underline t)))
   '(lsp-face-highlight-write   ((t :underline t)))
   '(lsp-modeline-code-actions-face ((t :foreground unspecified)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-branch-current     ((t :inherit magit-branch-local  :box (:line-width (1 . -1)))))
   '(magit-branch-remote-head ((t :inherit magit-branch-remote :box (:line-width (1 . -1)))))
   `(magit-diff-context ((t :foreground ,(color-from 'default :foreground 15) :weight normal)))
   '(magit-diff-context-highlight ((t :weight normal)))
   '(magit-diff-added ((t :background "#cceecc" :foreground "#119911")))
   '(magit-diff-added-highlight ((t :inherit magit-diff-added :background "#ddffdd")))
   '(magit-diff-removed ((t :background "#eecccc" :foreground "#aa2222")))
   '(magit-diff-removed-highlight ((t :inherit magit-diff-removed :background "#ffdddd")))
   '(magit-section-highlight ((t :box unspecified)))))

(use-package org
  :defer t
  :init
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
   '(org-punctuation-face ((t :inherit default :weight normal)))
   '(org-special-keyword ((t :weight normal)))
   `(org-task-done ((t :foreground ,(color-from 'org-headline-done :foreground) :weight normal)))
   '(org-todo ((t :box (:line-width (1 . -1)))))))

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

(use-package powerline
  :defer t
  :config
  (custom-set-faces
   '(powerline-active1 ((t :background "#edf8fc" :foreground "#335ea8")))))

(use-package spaceline
  :defer t
  :config
  (custom-set-faces
   '(spaceline-flycheck-error ((t :foreground "#ff8c8c" :distant-foreground "#c94976")))
   `(spaceline-symbol-segment-face ((t :inherit powerline-active1 :weight normal)))))

(use-package swiper
  :defer t
  :config
  (custom-set-faces
   '(swiper-line-face ((t :background "black" :foreground "white" :weight bold)))
   '(swiper-match-face-1 ((t :foreground "#d3d3d3")))
   '(swiper-match-face-2 ((t :foreground "#e99ce8")))
   '(swiper-match-face-3 ((t :foreground "#bbbbff")))
   '(swiper-match-face-4 ((t :foreground "#ffbbff")))
   '(swiper-background-match-face-1 ((t :background "#d3d3d3" :foreground "black" :weight normal)))
   '(swiper-background-match-face-2 ((t :background "#e99ce8" :foreground "black" :weight normal)))
   '(swiper-background-match-face-3 ((t :background "#bbbbff" :foreground "black" :weight normal)))
   '(swiper-background-match-face-4 ((t :background "#ffbbff" :foreground "black" :weight normal)))))

(use-package web-mode
  :defer t
  :config
  (custom-set-faces
   '(web-mode-html-attr-equal-face ((t :inherit shadow)))))

(use-package which-key
  :defer t
  :config
  (custom-set-faces
   `(which-key-command-description-face ((t :foreground ,(color-from 'font-lock-function-name-face :foreground))))))

(use-package yasnippet
  :defer t
  :config
  (custom-set-faces
   '(yas-field-highlight-face ((t :box (:line-width (-1 . -1) :color "#838383"))))))

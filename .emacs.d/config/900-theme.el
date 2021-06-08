;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (setq frame-background-mode 'dark)
  (load-theme 'spacemacs-dark t)

  (custom-set-faces
   '(c-style-brace-face ((t :inherit shadow :weight light)))
   '(c-style-operator-face ((t :weight normal)))
   '(c-style-punctuation-1-face ((t :inherit shadow)))
   '(c-style-punctuation-2-face ((t :inherit shadow :weight light)))
   '(bold ((t :foreground unspecified)))
   `(isearch ((t :underline unspecified)))
   '(italic ((t :foreground unspecified :family "Fantasque Sans Mono" :slant italic)))
   `(line-number ((t :weight light)))
   '(line-number-current-line ((t :background unspecified)))
   '(shadow ((t :weight normal)))
   '(symbol-dash-or-underline-face ((t :weight normal)))
   '(whitespace-newline ((t :inherit whitespace-tab :foreground unspecified)))
   '(whitespace-space   ((t :inherit whitespace-tab :foreground unspecified)))
   '(whitespace-tab     ((t :weight light)))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((t :background ,(color-from 'default :background (* 3 (if (eq frame-background-mode 'dark) -1 1)))
         :foreground ,(color-from 'default :foreground (* 5 (if (eq frame-background-mode 'dark) -1 1))))))
   `(auto-dim-other-line-number-face
     ((t :background ,(color-from 'line-number :background (* 3 (if (eq frame-background-mode 'dark) -1 1)))
         :foreground ,(color-from 'line-number :foreground (* 5 (if (eq frame-background-mode 'dark) -1 1))))))))

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
     `(clojure-cond-condtion-face ((t :underline (:color ,(color-from 'default :foreground (* 30 (if (eq frame-background-mode 'dark) -1 1)))))))
     '(clojure-define-type-face ((t :inherit font-lock-type-face :background "#3d1824")))
     '(clojure-defining-spec-face ((t :inherit font-lock-function-name-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit clojure-fn-parameter-face :weight normal)))
     `(clojure-important-keywords-face
       ((t :inherit font-lock-keyword-face :weight unspecified
           :underline (:color ,(color-from 'font-lock-keyword-face :foreground (* 10 (if (eq frame-background-mode 'dark) -1 1)))))))
     '(clojure-interop-method-face ((t :inherit font-lock-keyword-face :weight bold)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     '(clojure-ns-definition-face ((t :inherit font-lock-type-face :background "#3d1824")))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-local-binding-variable-name-unsed-face ((t :inherit clojure-local-binding-variable-name-face :weight normal)))
     '(clojure-punctuation-face ((t :inherit shadow :weight light)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face)))
     '(clojure-side-effect-face ((t :weight bold :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     `(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face
                                            :background "#27304a"
                                            :weight ,(face-attribute 'font-lock-function-name-face :weight)))))))

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
     '(lisp-punctuation-face ((t :inherit shadow :weight light)))
     '(lisp-use-package-face ((t :inherit font-lock-constant-face :background "#311b33"))))))

(use-package eshell
  :defer t
  :config
  (custom-set-faces
   '(eshell-prompt ((t :inherit shadow :weight light)))))

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused ((t :foreground ,(color-from 'default :foreground (* 25 (if (eq frame-background-mode 'dark) -1 1))))))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :inherit italic :weight light)))
   '(font-lock-doc-face ((t :inherit italic :weight light)))
   '(font-lock-function-name-face ((t :background "#38213b" :weight bold)))
   '(font-lock-keyword-face ((t :weight unspecified)))
   '(font-lock-negation-char-face ((t :inherit font-lock-warning-face :foreground unspecified)))
   '(font-lock-preprocessor-face ((t :weight normal)))
   '(font-lock-regexp-grouping-backslash ((t :weight normal)))
   '(font-lock-regexp-grouping-construct ((t :weight normal)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((t :weight unspecified))))
  (custom-set-faces
   `(font-lock-comment-delimiter-face
     ((t :foreground ,(color-from 'font-lock-comment-face :foreground (* 10 (if (eq frame-background-mode 'dark) -1 1))) :weight light)))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   `(fringe ((t :background "#2f343f")))))

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
        (->> (--iterate (dim-color it 5) (color-from 'show-paren-match :foreground) 6)
             (-drop 2)))

  (custom-set-faces
   '(highlight-parentheses-highlight ((t :weight bold :underline t)))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((t :weight bold)))
   '(ivy-grep-info ((t :inherit font-lock-string-face :weight light)))
   '(ivy-grep-line-number ((t :inherit compilation-line-number :weight light)))
   '(ivy-modified-buffer ((t :inherit highlight)))
   '(ivy-modified-outside-buffer ((t :inherit ivy-modified-buffer)))
   '(ivy-virtual ((t :inherit default)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-details-face ((t :inherit shadow :height 0.9)))
   '(lsp-modeline-code-actions-face ((t :foreground unspecified)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-branch-current     ((t :inherit magit-branch-local  :box (:line-width (1 . -1)))))
   '(magit-branch-remote-head ((t :inherit magit-branch-remote :box (:line-width (1 . -1)))))))

(use-package org
  :defer t
  :init
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (add-to-list 'face-remapping-alist
                           `(org-tag (:inherit default :foreground ,(color-from 'org-tag :foreground) :weight normal)))))

  :config
  (custom-set-faces
   '(org-done ((t :box (:line-width (1 . -1)))))
   '(org-drawer ((t :weight normal)))
   '(org-link ((t :weight unspecified)))
   '(org-punctuation-face ((t :inherit default :weight normal)))
   '(org-special-keyword ((t :weight normal)))
   '(org-tag ((t :background unspecified :slant unspecified)))
   `(org-task-done ((t :inherit shadow)))
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

(use-package spaceline
  :defer t
  :config
  (custom-set-faces
   `(spaceline-symbol-segment-face ((t :inherit powerline-active1 :weight normal)))))

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
   '(yas-field-highlight-face ((t :box (:line-width (-1 . -1) :color "#807c84"))))))

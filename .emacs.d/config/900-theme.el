;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package spacemacs-theme
  :ensure t
  :defer t)

(setq frame-background-mode 'dark)
(load-theme 'spacemacs-dark t)

(custom-set-faces
 '(bold ((t :foreground unspecified :weight bold)))
 '(c-style-brace-face ((t :inherit shadow :weight light)))
 '(c-style-operator-face ((t :weight normal)))
 '(c-style-punctuation-1-face ((t :inherit shadow)))
 '(c-style-punctuation-2-face ((t :inherit shadow :weight light)))
 '(default ((t :background "#242528")))
 '(fixed-pitch ((t :family "Fantasque Sans Mono")))
 '(fringe  ((t :background "#2f343f" :foreground "#585c65")))
 `(isearch ((t :underline unspecified :weight bold)))
 '(italic ((t :foreground unspecified :family "Fantasque Sans Mono" :slant italic)))
 '(lazy-highlight ((t :weight bold)))
 '(line-number-current-line ((t :background unspecified)))
 '(shadow ((t :weight normal)))
 '(show-paren-match ((t :weight bold)))
 '(symbol-dash-or-underline-face ((t :weight normal)))
 '(vertical-border ((t :foreground "#2f343f")))
 '(whitespace-newline ((t :inherit whitespace-tab :foreground unspecified)))
 '(whitespace-space   ((t :inherit whitespace-tab :foreground unspecified)))
 '(whitespace-tab ((t :weight light))))
(custom-set-faces
 `(line-number ((t :background ,(color-from 'fringe :background (* 5 (if (eq frame-background-mode 'dark) -1 1))) :weight light))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((t :background ,(color-from 'default :background (*  2 (if (eq frame-background-mode 'dark) -1 1)))
         :foreground ,(color-from 'default :foreground (* 10 (if (eq frame-background-mode 'dark) -1 1))))))
   `(auto-dim-other-line-number-face
     ((t :background ,(color-from 'line-number :background (* 2 (if (eq frame-background-mode 'dark) -1 1)))
         :foreground ,(color-from 'line-number :foreground (* 5 (if (eq frame-background-mode 'dark) -1 1))))))))

(use-package cider-mode
  :defer t
  :config
  (custom-set-faces
   '(cider-reader-conditional-face ((t :inherit shadow)))
   `(cider-deprecated-face  ((t :underline (:style line :color "darkorange"))))
   '(cider-fringe-good-face ((t :inherit success)))
   '(cider-repl-prompt-face ((t :inherit font-lock-keyword-face :weight light)))
   '(cider-repl-stdout-face ((t :inherit font-lock-string-face :weight light)))))

(use-package clojure-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(clojure-cond-condtion-face ((t :inherit italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face :background "#3d1824")))
     '(clojure-defining-spec-face ((t :inherit font-lock-keyword-face :background "#1c354d")))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit clojure-fn-parameter-face :weight normal)))
     `(clojure-important-keywords-face
       ((t :inherit font-lock-keyword-face :weight unspecified
           :background ,(color-from 'default :background (* 4 (if (eq frame-background-mode 'dark) 1 -1))))))
     '(clojure-interop-method-face ((t :inherit font-lock-keyword-face :weight bold)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
     '(clojure-ns-prefix-face ((t :inherit font-lock-type-face :weight normal)))
     '(clojure-meta-face ((t :inherit shadow :weight normal)))
     '(clojure-ns-definition-face ((t :inherit font-lock-type-face :background "#3d1824")))
     `(clojure-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :foreground "#6981c5" :weight ,default-weight)))
     '(clojure-local-binding-variable-name-unsed-face ((t :inherit clojure-local-binding-variable-name-face :weight normal)))
     '(clojure-punctuation-face ((t :inherit shadow :weight light)))
     '(clojure-semi-function-name-face ((t :inherit font-lock-function-name-face)))
     '(clojure-side-effect-face ((t :weight bold :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     `(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face
                                            :background "#2b3552"
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
     `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face :foreground "#6981c5" :weight ,default-weight)))
     '(lisp-punctuation-face ((t :inherit shadow :weight light)))
     '(lisp-use-package-face ((t :inherit font-lock-constant-face :background "#3d2340"))))))

(use-package eshell
  :defer t
  :config
  (custom-set-faces
   '(epe-symbol-face ((t :inherit shadow)))
   '(epe-git-face ((t :inherit font-lock-constant-face :weight normal)))
   '(eshell-prompt ((t :inherit shadow :weight light)))))

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused ((t :foreground ,(color-from 'default :foreground (* 25 (if (eq frame-background-mode 'dark) -1 1))) :weight light)))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :inherit italic :background unspecified :weight light :slant unspecified)))
   '(font-lock-doc-face ((t :inherit italic :weight light)))
   '(font-lock-function-name-face ((t :background "#412645" :weight bold)))
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
   '(ivy-modified-buffer ((t :foreground "#dc752f")))
   '(ivy-modified-outside-buffer ((t :inherit ivy-modified-buffer)))
   '(ivy-virtual ((t :inherit default)))))

(use-package ivy-posframe
  :defer t
  :config
  (custom-set-faces
   `(ivy-posframe-cursor ((t :inherit cursor :foreground ,(color-from 'default :background))))
   `(ivy-swiper-line-number  ((t :inherit line-number :background ,(color-from 'ivy-posframe :background 5)))))

  (advice-add #'ivy-posframe--add-prompt :after
              ;; NOTE
              ;;  If the font size is larger than 10.0, the heigh of '▏' character is longer than other character.
              ;;  characters in `posframe' is moving when switching to `evil' normal mode.
              ;;  See, `ivy-posframe--custom-add-prompt' in 500-eye-candy.el.
              (let ((mapping `((minibuffer-prompt (:inherit bold :foreground ,(color-from 'minibuffer-prompt :foreground) :overline ,(color-from 'default :background))))))
                (lambda (&rest _)
                  "To overwrite `minibuffer-prompt' face."
                  (with-current-buffer ivy-posframe-buffer
                    (setq-local face-remapping-alist mapping))))))

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
   '(org-agenda-calendar-event ((t :inherit unspecified)))
   '(org-agenda-date ((t :weight normal :height 1.3)))
   '(org-agenda-date-today ((t :background "#1c354d" :height 1.3)))
   '(org-agenda-date-weekend ((t :weight normal :height 1.3)))
   '(org-block ((t :inherit fixed-pitch)))
   '(org-block-begin-line ((t :weight normal)))
   '(org-block-end-line   ((t :weight normal)))
   '(org-code ((t :foreground "#24c7d8")))
   '(org-done ((t :inherit shadow :foreground unspecified :box (:line-width (1 . -1)))))
   '(org-headline-done ((t :inherit shadow)))
   '(org-drawer ((t :weight normal)))
   '(org-ellipsis ((t :inherit shadow :foreground unspecified)))
   '(org-link ((t :weight unspecified)))
   '(org-punctuation-face ((t :inherit default :weight normal)))
   '(org-scheduled-previously ((t :inherit (shadow italic) :foreground unspecified :weight normal :slant unspecified)))
   '(org-special-keyword ((t :weight normal)))
   '(org-super-agenda-header ((t :foreground "#2d9574" :weight normal :height 1.3)))
   '(org-tag ((t :background unspecified :slant unspecified)))
   `(org-task-done ((t :inherit shadow)))
   '(org-todo ((t :box (:line-width (1 . -1)))))
   `(org-upcoming-distant-deadline
     ((t :inherit org-upcoming-deadline
         :foreground ,(color-from 'org-upcoming-deadline :foreground (* 10 (if (eq frame-background-mode 'dark) -1 1))))))))

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

(use-package smartparens
  :defer t
  :config
  (custom-set-faces
   '(sp-show-pair-match-face ((t :weight bold)))))

(use-package spaceline
  :defer t
  :config
  (custom-set-faces
   `(spaceline-symbol-segment-face ((t :inherit powerline-active1 :weight normal)))))

(use-package treemacs
  :defer t
  :config
  (custom-set-faces
   '(treemacs-git-ignored-face ((t :inherit shadow :foreground unspecified)))))

(use-package web-mode
  :defer t
  :config
  (custom-set-faces
   '(web-mode-html-attr-equal-face ((t :inherit shadow)))
   '(web-mode-html-tag-bracket-face ((t :inherit shadow)))))

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

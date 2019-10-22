(use-package leuven-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground 7) -20)
    "TODO")
  (setq x-underline-at-descent-line t)
  (custom-set-faces
   `(font-lock-comment-face ((t (:slant normal))))
   `(fringe ((t (:background "#F8F8F8"))))
   `(isearch ((t (:inherit bold :underline unspecified))))
   `(link ((t (:inherit underline :underline unspecified))))
   `(link-visited ((t (:inherit underline :underline unspecified))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium)))))
  (custom-theme-set-faces
   'leuven
   `(bold ((t (:weight ultrabold))))
   `(italic ((t (:slant italic))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#7F7F7F"))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-warning-face :weight medium))))
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-match-selection ((t (:inherit isearch))))
   `(helm-selection ((t (:background ,(color-from 'isearch :background 35) :distant-foreground "black"))))
   `(underline ((t (:underline (:color foreground-color :style wave)))))
   `(variable-pitch ((t (:family "DejaVu Serif"))))))


;; customize color theme for minor/major modes

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background "#FAFAFA"))))))

(use-package creole-mode
  :defer t
  :config
  (custom-theme-set-faces
   'leuven
   `(info-title-1 ((t (:inherit outline-1 :height 1.3))))
   `(info-title-2 ((t (:inherit outline-2 :height 1.2))))
   `(info-title-3 ((t (:inherit outline-3 :height 1.1))))
   `(info-title-4 ((t (:inherit outline-4 :height 1.0))))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit font-lock-constant-face))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   `(elixir-argument-name-face ((t (:inherit font-lock-variable-name-face))))
   `(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil-goggles
  :defer t
  :config
  (evil-goggles-use-diff-refine-faces))

(use-package go-mode
  :defer t
  :config
  (custom-set-faces
   `(go-argument-name-face ((t (:inherit font-lock-variable-name-face))))))

(use-package highlight-parentheses
  :defer t
  :init
  (setq hl-paren-base-color "light sea green")

  :config
  (setq hl-paren-colors
        (--iterate (dim-color it 6)
                   (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb hl-paren-base-color))
                   4)))

(use-package highlight-symbol
  :defer t
  :config
  (custom-theme-set-faces
   'leuven
   `(highlight-symbol-face ((t (:background "#FFFFCF"))))))

(use-package hl-line
  :defer t
  :config
  (custom-set-faces
   `(hl-line ((t (:underline unspecified :inverse-video nil))))))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t (:inherit default))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   `(magit-section-highlight ((t (:background ,(color-from 'isearch :background 35) :distant-foreground "black")))))
  (with-eval-after-load "hl-line"
    (custom-set-faces
     `(magit-diff-context-highlight ((t (:background ,(color-from 'hl-line :background 7) :foreground ,(color-from 'hl-line :background -50))))))))

(use-package make-mode
  :defer t
  :config
  (custom-set-faces
   `(makefile-targets ((t (:weight normal))))))

(use-package org
  :defer t
  :config
  (custom-set-faces
   `(org-agenda-date ((t (:height 1.3))))
   `(org-agenda-date-today ((t (:height 1.3))))
   `(org-agenda-date-weekend ((t (:height 1.3))))
   `(org-block-begin-line ((t (:underline unspecified))))
   `(org-block-end-line ((t (:overline unspecified))))
   `(org-date ((t (:inherit italic :underline unspecified))))
   `(org-level-1 ((t (:height 1.0))))
   `(org-link ((t (:inherit link :underline unspecified))))
   `(org-quote ((t (:slant normal))))
   `(org-tag ((t (:background unspecified))))
   `(org-warning ((t (:background "#FFDDDD" :foreground "#FF5555")))))
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :inherit 'bold))))

(use-package paren
  :defer t
  :config
  (custom-set-faces
   `(show-paren-match    ((t (:inherit bold :foreground ,(light-color hl-paren-base-color 5) :underline t))))
   `(show-paren-mismatch ((t (:inherit (bold font-lock-warning-face)))))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:inherit font-lock-variable-name-face))))
   `(php-variable-name ((t (:foreground ,(-> 'font-lock-variable-name-face
                                             (color-from :foreground 30)
                                             (saturate-color 10)
                                             (mix-color (color-from 'font-lock-string-face :foreground -30)))))))))

(use-package rainbow-delimiters
  :defer t
  :config
  (dolist (i (number-sequence 1 9))
    (let ((face (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))))
      (set-face-attribute face nil :foreground
                          (-> face
                              (face-attribute :foreground)
                              (dim-color 10)
                              (saturate-color -20))))))

(use-package rpm-spec-mode
  :defer t
  :config
  (set-face-attribute 'rpm-spec-section-face nil
                      :underline 'unspecified
                      :inherit 'underline))

(use-package smartparens
  :defer t
  :config
  (custom-set-faces
   `(sp-show-pair-match-face    ((t (:inherit bold :foreground ,(light-color hl-paren-base-color 5) :underline t))))
   `(sp-show-pair-mismatch-face ((t (:inherit (bold font-lock-warning-face)))))))

(use-package web-mode
  :disabled t
  :defer t
  :config
  (let ((tag-face       `(:foreground ,(-> "#AE1B9A" (light-color 15) (saturate-color -40))))
        (attr-name-face `(:foreground ,(-> "#F36335" (light-color 15) (saturate-color -40))))
        (attr-var-face  `(:foreground ,(-> "green4"  (light-color 15) (saturate-color -40)))))
   (custom-set-faces
    `(web-mode-html-tag-bracket-face ((t ,tag-face)))
    `(web-mode-html-tag-face ((t ,tag-face)))
    `(web-mode-html-attr-name-face ((t ,attr-name-face)))
    `(web-mode-html-attr-value-face ((t ,attr-var-face))))))

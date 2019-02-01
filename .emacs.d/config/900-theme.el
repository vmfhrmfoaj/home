(use-package leuven-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground 7) -20)
    "TODO")
  (setq x-underline-at-descent-line t)
  (custom-set-faces
   `(bold ((t (:weight bold))))
   `(font-lock-comment-face ((t (:slant normal))))
   `(fringe ((t (:background "#F8F8F8"))))
   `(isearch ((t (:underline unspecified :weight bold))))
   `(link ((t (:inherit underline :underline unspecified))))
   `(link-visited ((t (:inherit underline :underline unspecified))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium)))))
  (custom-theme-set-faces
   'leuven
   `(bold ((t (:weight bold))))
   `(italic ((t (:slant italic))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#7F7F7F"))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-warning-face :weight medium))))
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-match-selection ((t (:inherit isearch))))
   `(helm-selection ((t (:background ,(color-from 'isearch :background 35) :distant-foreground "black"))))
   `(underline ((t (:underline (:color foreground-color :style wave)))))
   `(variable-pitch ((t (:family "DejaVu Serif"))))))

(use-package auto-dim-other-buffers
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background "#FAFAFA"))))))

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

(use-package evil
  :defer t
  :config
  (setq-default cursor-in-non-selected-windows nil)
  (let* ((hbar-height (max line-spacing 2))
         (hbar-height (if (string-equal "gnome-imac" hostname)
                          (* 2 hbar-height) ; for HiDPI
                        hbar-height))
         (default-color (color-from 'cursor :background   0))
         (visual-color  (color-from 'cursor :background -15))
         (operator-color (color-from 'font-lock-keyword-face :foreground 0))
         (replace-color  (color-from 'font-lock-warning-face :foreground 0)))
    (setq-default cursor-type 'bar)
    (setq evil-normal-state-cursor   `((hbar . ,hbar-height) ,default-color)
          evil-visual-state-cursor   `((hbar . ,hbar-height) ,visual-color)
          evil-operator-state-cursor `((hbar . ,hbar-height) ,operator-color)
          evil-insert-state-cursor   `(bar ,default-color)
          evil-replace-state-cursor  `((hbar . ,hbar-height) ,replace-color))
    (with-eval-after-load 'evil-multiedit
      (setq evil-multiedit-normal-state-cursor `((hbar . ,hbar-height) ,default-color)
            evil-multiedit-insert-state-cursor `(bar ,default-color)))))

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
   `(hl-line ((t (:underline unspecified :inverse-video nil)))))
  (cond
   ((string-equal "gnome-macbookair" hostname)
    (custom-set-faces
     `(hl-line ((t (:background "#F5FECA" :underline unspecified :inverse-video nil))))))))

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
   `(org-tag ((t (:background unspecified))))))

(use-package paren
  :defer t
  :config
  (custom-set-faces
   `(show-paren-match    ((t (:foreground ,(light-color hl-paren-base-color 5) :weight bold :underline t))))
   `(show-paren-mismatch ((t (:inherit font-lock-warning-face :weight bold))))))

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

(use-package smartparens
  :defer t
  :config
  (custom-set-faces
   `(sp-show-pair-match-face    ((t (:foreground ,(light-color hl-paren-base-color 5) :weight bold :underline t))))
   `(sp-show-pair-mismatch-face ((t (:inherit font-lock-warning-face :weight bold))))))

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

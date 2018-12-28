(use-package leuven-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground 7) -20)
    "TODO")
  (setq x-underline-at-descent-line t)
  (custom-set-faces
   `(bold ((t (:weight bold))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit font-lock-constant-face))))
   `(elixir-argument-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(fringe ((t (:background "#FEFEFE"))))
   `(hl-line ((t (:underline unspecified :inverse-video nil))))
   `(hl-paren-face ((t (:weight bold))))
   `(isearch ((t (:underline unspecified :weight bold))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(org-date ((t (:underline unspecified :slant italic))))
   `(org-link ((t (:inherit underline :underline unspecified)))))
  (custom-theme-set-faces
   'leuven
   `(bold ((t (:weight bold))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "PaleGreen4"))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-warning-face :weight medium))))
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-match-selection ((t (:inherit isearch))))
   `(helm-selection ((t (:background ,(color-from 'isearch :background 30) :distant-foreground "black" :weight medium))))
   `(underline ((t (:underline (:color foreground-color :style wave)))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   `(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil
  :defer t
  :config
  (let* ((hbar-height (max line-spacing 2))
         (hbar-height (if (string-equal "gnome-imac" hostname)
                          (* 2 hbar-height) ; for HiDPI
                        hbar-height))
         (default-color (color-from 'cursor :background   0))
         (visual-color  (color-from 'cursor :background -15))
         (operator-color "#A155E7")
         (replace-color  "#DD9393"))
    (setq cursor-type 'box
          evil-normal-state-cursor   `(box ,default-color)
          evil-visual-state-cursor   `(hollow ,visual-color)
          evil-operator-state-cursor `(box ,operator-color)
          evil-insert-state-cursor   `(bar ,default-color)
          evil-replace-state-cursor  `((hbar . ,hbar-height) ,replace-color))
    (with-eval-after-load 'evil-multiedit
      (setq evil-multiedit-normal-state-cursor `(box ,default-color)
            evil-multiedit-insert-state-cursor `(bar ,default-color)))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-base-color "light sea green"
        hl-paren-colors
        (--iterate (dim-color it 6)
                   (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb hl-paren-base-color))
                   4)))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t (:inherit default))))))

(use-package org
  :defer t
  :config
  (custom-set-faces
   `(variable-pitch ((t (:font-family "Monospace")))))
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :inherit 'variable-pitch))))

(use-package paren
  :after highlight-parentheses
  :config
  (custom-set-faces
   `(show-paren-match    ((t (:foreground ,hl-paren-base-color :weight bold :underline (:color "#D0372D")))))
   `(show-paren-mismatch ((t (:weight bold))))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:foreground ,local-variable-name-fg-color))))))

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
  :after highlight-parentheses
  :config
  (custom-set-faces
   `(sp-show-pair-match-face    ((t (:foreground ,hl-paren-base-color :weight bold :underline (:color "#D0372D")))))
   `(sp-show-pair-mismatch-face ((t (:weight bold))))))

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

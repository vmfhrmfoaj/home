(use-package zenburn-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (color-from 'font-lock-variable-name-face :foreground 0)
    "TODO")
  (setq x-underline-at-descent-line t)
  (load-theme 'zenburn t)
  (custom-set-faces
   `(bold ((t (:weight bold))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(font-lock-doc-face ((t (:slant italic))))
   `(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:weight bold))))
   `(helm-selection ((t (:weight bold :underline unspecified))))
   `(lazy-highlight ((t (:background "#333333"))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(org-done ((t (:underline t))))
   `(org-todo ((t (:underline t))))
   `(region ((t (:background "#DCDCCC" :foreground "#3F3F3F" :distant-foreground "#3F3F3F" :weight medium :underline nil :box nil :overline nil :slant normal :inverse-video nil)))))
  (let ((diff-added-bg+5    (color-from 'diff-added   :background  5))
        (diff-added-bg+15   (color-from 'diff-added   :background 15))
        (diff-added-fg+5    (color-from 'diff-added   :foreground  5))
        (diff-added-fg+15   (color-from 'diff-added   :foreground 15))
        (diff-removed-bg+5  (color-from 'diff-removed :background  5))
        (diff-removed-fg+5  (color-from 'diff-removed :foreground  5))
        (diff-removed-bg+15 (color-from 'diff-removed :background 15))
        (diff-removed-fg+15 (color-from 'diff-removed :foreground 15))
        (diff-changed-bg+5  (color-from 'diff-changed :background  5))
        (diff-changed-bg+15 (color-from 'diff-changed :background 15))
        (diff-changed-fg+5  (color-from 'diff-changed :foreground  5))
        (diff-changed-fg+15 (color-from 'diff-changed :foreground 15)))
   (custom-theme-set-faces
    'zenburn
    `(diff-refine-added   ((t (:inherit diff-added   :background ,diff-added-bg+15 :foreground ,diff-added-fg+15))))
    `(diff-refine-changed ((t (:inherit diff-removed :background ,diff-changed-bg+15 :foreground ,diff-changed-fg+15))))
    `(diff-refine-removed ((t (:inherit diff-removed :background ,diff-removed-bg+15 :foreground ,diff-removed-fg+15))))
    `(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
    `(evil-goggles-paste-face  ((t (:inherit diff-refine-added))))
    `(evil-goggles-yank-face   ((t (:inherit diff-refine-changed))))
    `(helm-match ((t (:inherit lazy-highlight))))
    `(helm-match-selection ((t (:inherit isearch))))
    `(helm-selection ((t (:background "#5F5F5F"))))
    `(helm-swoop-target-line-face ((t (:inherit isearch))))
    `(helm-swoop-target-word-face ((t (:inherit helm-match))))
    `(isearch ((t (:foreground "#DCCFAB" :background "#1F1F1F"))))
    `(magit-diff-added-highlight   ((t (:inherit diff-added   :background ,diff-added-bg+5 :foreground ,diff-added-fg+5))))
    `(magit-diff-file-heading-highlight ((t (:background "#5F5F5F"))))
    `(magit-diff-removed-highlight ((t (:inherit diff-removed :background ,diff-removed-bg+5 :foreground ,diff-removed-fg+5))))
    `(org-scheduled-previously ((t (:inherit default))))
    `(trailing-whitespace ((t (:underline "#CC9393"))))
    `(whitespace-space ((t (:foreground "#5F5F5F")))))
    `(whitespace-tab   ((t (:foreground "#5F5F5F"))))
    `(whitespace-trailing ((t (:underline "#CC9393"))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   `(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil
  :defer t
  :config
  (let* ((hbar-height (max line-spacing 1))
         (hbar-height (if (string-equal "gnome-imac" hostname)
                          (* 2 hbar-height) ; for HiDPI
                        hbar-height))
         (default-color "#DCDCCC")
         (visual-color "#000000")
         (operator-color "#B967FF")
         (replace-color "#B967FF"))
    (setq cursor-type 'box
          evil-normal-state-cursor   `(box ,default-color)
          evil-visual-state-cursor   `(hollow ,visual-color)
          evil-operator-state-cursor `(box ,operator-color)
          evil-insert-state-cursor   `(bar ,default-color)
          evil-replace-state-cursor  `(bar ,replace-color))
    (with-eval-after-load 'evil-multiedit
      (setq evil-multiedit-normal-state-cursor `(box ,default-color)
            evil-multiedit-insert-state-cursor `(bar ,default-color)))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-base-color "Springgreen1"
        hl-paren-colors
        (--iterate (dim-color it 7)
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
   `(show-paren-match    ((t (:foreground ,hl-paren-base-color :wiehgt bold))))
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
   `(sp-show-pair-match-face    ((t (:foreground ,hl-paren-base-color :wiehgt bold))))
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

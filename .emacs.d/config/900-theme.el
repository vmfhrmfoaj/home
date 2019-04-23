(use-package solarized-theme
  :ensure t
  :init
  (defvar local-variable-name-fg-color
    (-> 'font-lock-variable-name-face
        (color-from :foreground 7)
        (saturate-color -20))
    "TODO")

  (setq solarized-high-contrast-mode-line t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        x-underline-at-descent-line t)

  :config
  (load-theme 'solarized-light t)
  (custom-set-faces
   `(fringe ((t (:background "#F5EFDC"))))
   `(isearch ((t (:underline unspecified))))
   `(link ((t (:inherit underline :underline unspecified :weight unspecified))))
   `(link-visited ((t (:inherit underline :underline unspecified)))))
  (custom-theme-set-faces
   'solarized-light
   `(bold ((t (:weight ultrabold))))
   `(cursor ((t (:background "#657b83"))))
   `(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
   `(evil-goggles-paste-face  ((t (:inherit diff-refine-added))))
   `(evil-goggles-yank-face   ((t (:inherit diff-refine-changed))))
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-match-selection ((t (:inherit isearch))))
   `(helm-selection ((t (:inherit hl-line))))
   `(italic ((t (:slant italic))))
   `(trailing-whitespace ((t (:underline (:color "#DC322F" :style wave)))))
   `(underline ((t (:underline (:color foreground-color :style wave)))))
   `(variable-pitch ((t (:family "DejaVu Serif"))))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background "#F7F1DE"))))))

(use-package creole-mode
  :defer t
  :config
  (custom-theme-set-faces
   'solarized-light
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

(use-package elisp-mode
  :defer t
  :config
  (custom-set-faces
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium))))))

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
  (when window-system
    (let* ((hbar-height (max line-spacing 2))
           (hbar-height (if (string-equal "gnome-imac" hostname)
                            (floor (* 1.4 hbar-height)) ; for HiDPI
                          hbar-height))
           (default-color (color-from 'default :foreground))
           (search-color  (color-from 'isearch :background))
           (pending-color (color-from 'font-lock-keyword-face :foreground))
           (replace-color (color-from 'font-lock-warning-face :foreground))
           (special-color (color-from 'font-lock-constant-face :foreground)))
      (setq-default cursor-type 'bar)
      (setq evil-normal-state-cursor   `((hbar . ,hbar-height) ,default-color)
            evil-visual-state-cursor   `((hbar . ,(1+ hbar-height)) ,search-color)
            evil-operator-state-cursor `((hbar . ,(1+ hbar-height)) ,pending-color)
            evil-insert-state-cursor   `(bar ,default-color)
            evil-replace-state-cursor  `((hbar . ,(1+ hbar-height)) ,replace-color)
            evil-emacs-state-cursor    `(bar ,special-color))
      (with-eval-after-load 'evil-multiedit
        (setq evil-multiedit-state-cursor        `((hbar . ,hbar-height) "#E072A7")
              evil-multiedit-insert-state-cursor `(bar "#E072A7"))
        (custom-set-faces
         `(iedit-occurrence ((t (:inherit region :background unspecified :foreground "#E072A7")))))))))

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
  (custom-set-faces
   `(hl-paren-face ((t (:inherit bold)))))
  (setq hl-paren-colors
        (--iterate (dim-color it 6)
                   (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb hl-paren-base-color))
                   4)))

(use-package highlight-symbol
  :defer t
  :config
  (custom-theme-set-faces
   'solarized-light
   `(highlight-symbol-face ((t (:background ,(color-from 'default :background -3)))))))

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
   `(linum ((t (:inherit default :background "#EEE8D5"))))))

(use-package org
  :defer t
  :config
  (custom-set-faces
   `(org-block ((t (:background ,(color-from 'default :background -2)))))
   `(org-block-begin-line ((t (:inherit org-meta-line :background ,(color-from 'default :background -1) :underline unspecified))))
   `(org-block-end-line   ((t (:inherit org-meta-line :background ,(color-from 'default :background -1) :overline  unspecified))))
   `(org-checkbox ((t (:box unspecified))))
   `(org-date ((t (:underline unspecified))))
   `(org-done ((t (:inherit shadow :background unspecified :foreground unspecified :weight normal))))
   `(org-link ((t (:inherit link :underline unspecified))))
   `(org-verbatim ((t (:inherit variable-pitch)))))
  (-when-let (strikethrough (assoc "+" org-emphasis-alist))
    (setf (cdr strikethrough) '((:inherit shadow :strike-through t))))
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

(use-package powerline
  :defer t
  :config
  (custom-set-faces
   `(powerline-active0   ((t (:inherit mode-line))))
   `(powerline-active1   ((t (:inherit mode-line))))
   `(powerline-active2   ((t (:inherit mode-line))))
   `(powerline-inactive0 ((t (:inherit mode-line-inactive))))
   `(powerline-inactive1 ((t (:inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:inherit mode-line-inactive))))))

(use-package spaceline
  :defer t
  :config
  (custom-set-faces
   `(spaceline-highlight-face ((t (:inherit mode-line))))))

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

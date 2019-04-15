(use-package zenburn-theme
  :ensure t
  :init
  (setq x-underline-at-descent-line t
        zenburn-use-variable-pitch nil
        zenburn-scale-org-headlines nil
        zenburn-scale-outline-headlines nil)
  :config
  (defvar local-variable-name-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground 5) -20)
    "TODO")
  (custom-set-faces
   `(helm-selection ((t (:distant-foreground ,(color-from 'default :foreground 0)))))
   `(link ((t (:inherit underline :underline unspecified :weight unspecified))))
   `(link-visited ((t (:inherit underline :underline unspecified))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium)))))
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
    `(bold ((t (:weight ultrabold))))
    `(diff-refine-added   ((t (:inherit diff-added   :background ,diff-added-bg+15   :foreground ,diff-added-fg+15))))
    `(diff-refine-changed ((t (:inherit diff-removed :background ,diff-changed-bg+15 :foreground ,diff-changed-fg+15))))
    `(diff-refine-removed ((t (:inherit diff-removed :background ,diff-removed-bg+15 :foreground ,diff-removed-fg+15))))
    `(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
    `(evil-goggles-paste-face  ((t (:inherit diff-refine-added))))
    `(evil-goggles-yank-face   ((t (:inherit diff-refine-changed))))
    `(isearch ((t (:background "#DFAF8F" :foreground "#2B2B2B" :weight bold))))
    `(italic ((t (:slant italic))))
    `(magit-diff-added-highlight   ((t (:inherit diff-added   :background ,diff-added-bg+5   :foreground ,diff-added-fg+5))))
    `(magit-diff-removed-highlight ((t (:inherit diff-removed :background ,diff-removed-bg+5 :foreground ,diff-removed-fg+5))))
    `(underline ((t (:underline (:color foreground-color :style wave)))))
    `(variable-pitch ((t (:family "DejaVu Serif")))))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background "#383838"))))))

(use-package creole-mode
  :defer t
  :config
  (custom-theme-set-faces
   'zenburn
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

(use-package evil
  :defer t
  :config
  (setq-default cursor-in-non-selected-windows nil)
  (let* ((hbar-height (max line-spacing 2))
         (hbar-height (if (string-equal "gnome-imac" hostname)
                          (floor (* 1.4 hbar-height)) ; for HiDPI
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

(use-package org
  :defer t
  :config
  (custom-set-faces
   `(org-agenda-date ((t (:height 1.2))))
   `(org-agenda-date-today ((t (:inherit font-lock-function-name-face :foreground unspecified :height 1.2 :slant unspecified))))
   `(org-agenda-date-weekend ((t (:height 1.2))))
   `(org-block ((t (:inherit default :background "#323232"))))
   `(org-block-begin-line ((t (:foreground "grey40" :background "#363636"))))
   `(org-block-end-line   ((t (:foreground "grey40" :background "#363636"))))
   `(org-done ((t (:foreground "grey70" :weight normal))))
   `(org-link ((t (:inherit link :underline unspecified))))
   `(org-verbatim ((t (:inherit variable-pitch)))))
  (-when-let (strikethrough (assoc "+" org-emphasis-alist))
    (setf (cdr strikethrough) '((:inherit shadow :strike-through t))))
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :inherit 'bold))))

(use-package paren
  :after highlight-parentheses
  :defer t
  :config
  (custom-set-faces
   `(show-paren-match    ((t (:inherit bold :foreground ,(car hl-paren-colors) :background ,(saturate-color (car hl-paren-colors) -70)))))
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

(use-package rpm-spec-mode
  :defer t
  :config
  (set-face-attribute 'rpm-spec-section-face nil
                      :underline 'unspecified
                      :inherit 'underline))

(use-package smartparens
  :after highlight-parentheses
  :defer t
  :config
  (custom-set-faces
   `(sp-show-pair-match-face    ((t (:inherit bold :foreground ,(car hl-paren-colors) :background ,(saturate-color (car hl-paren-colors) -70)))))
   `(sp-show-pair-mismatch-face ((t (:inherit (bold font-lock-warning-face)))))))

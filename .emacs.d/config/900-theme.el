(use-package leuven-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (color-from 'font-lock-variable-name-face :foreground 5)
    "TODO")
  (setq x-underline-at-descent-line t)
  (custom-set-faces
   `(bold ((t (:weight bold))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(fringe ((t (:background "#FEFEFE"))))
   `(isearch ((t (:underline unspecified))))
   `(lazy-highlight ((t (:weight bold))))
   `(lisp-local-binding-variable-name-face ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(org-date ((t (:underline unspecified :slant italic))))
   `(org-link ((t (:inherit underline :underline unspecified)))))
  (custom-theme-set-faces
   'leuven
   `(bold ((t (:weight bold))))
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-selection ((t (:inherit isearch :weight bold))))
   `(hl-line ((t (:weight bold))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-warning-face :weight medium))))
   `(underline ((t (:underline (:color foreground-color :style wave)))))))


;; -- major/minor modes

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
                        hbar-height)))
    (setq cursor-type 'box
          evil-normal-state-cursor `(box ,(color-from 'cursor :background 0))
          evil-insert-state-cursor `((hbar . ,hbar-height))
          evil-visual-state-cursor `(box ,(color-from 'cursor :background -15))
          evil-replace-state-cursor `((hbar . ,hbar-height) "#DD9393")
          evil-operator-state-cursor `(box "#A155E7"))
    (with-eval-after-load 'evil-multiedit
      (setq evil-multiedit-state-cursor `(box ,(color-from 'cursor :background 0))
            evil-multiedit-insert-state-cursor `((hbar . ,hbar-height))))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-colors
        (--iterate (dim-color it 10)
                   (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb "Springgreen3"))
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
  :defer t
  :config
  (custom-set-faces
   `(show-paren-match    ((t (:foreground "Springgreen3" :wiehgt bold :underline (:color "orange red" :style line)))))
   `(show-paren-mismatch ((t (:weight bold))))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:foreground ,local-variable-name-fg-color))))))

(use-package smartparens
  :defer t
  :config
  (custom-set-faces
   `(sp-show-pair-match-face    ((t (:foreground "Springgreen3" :wiehgt bold :underline (:color "orange red" :style line)))))
   `(sp-show-pair-mismatch-face ((t (:weight bold))))))

(use-package web-mode
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

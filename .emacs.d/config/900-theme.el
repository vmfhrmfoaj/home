(custom-set-faces
 `(bold ((t (:weight bold))))
 `(cider-fringe-good-face ((t (:inherit success))))
 `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
 `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
 `(clojure-fn-parameter-face ((t (:inherit font-lock-variable-name-face :weight medium))))
 `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))
 `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
 `(clojure-side-effect-face ((t (:inherit (bold italic font-lock-warning-face)))))
 `(clojure-special-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
 `(css-property ((t (:inherit font-lock-builtin-face :foreground nil :weight medium))))
 `(css-selector ((t (:inherit (bold font-lock-variable-name-face) :foreground nil))))
 `(font-lock-comment-face ((t (:slant normal))))
 `(font-lock-doc-face ((t (:slant italic))))
 `(font-lock-function-name-face ((t (:inherit bold))))
 `(font-lock-string-face ((t (:slant normal))))
 `(font-lock-variable-name-face ((t (:inherit bold))))
 `(hl-line ((t (:inverse-video nil))))
 `(lisp-local-binding-variable-name-face ((t (:inherit font-lock-variable-name-face :weight medium))))
 `(mode-line ((t (:weight medium))))
 `(mode-line-inactive ((t (:inherit mode-line))))
 `(nlinum-current-line ((t (:inherit linum))))
 `(nlinum-relative-current-face ((t (:inherit linum))))
 `(org-agenda-date-today ((t (:inherit (bold org-agenda-date)))))
 `(org-mode-line-clock ((t)))
 `(widget-button ((t (:inherit bold)))))

(use-package twilight-anti-bright-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'twilight-anti-bright t)
  (custom-set-faces
   `(link ((t (:inherit underline :weight normal))))
   `(linum-relative-current-face ((t (:inherit linum))))
   `(magit-section-heading ((t (:inherit bold :foreground "DarkGoldenrod4"))))
   `(magit-diff-file-heading ((t (:weight medium))))
   `(magit-commit-log-type-face  ((t (:inherit font-lock-function-name-face :weight medium))))
   `(magit-commit-log-scope-face ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(show-paren-match ((t (:inherit underline :foreground "Cyan2" :background nil)))))
  (custom-set-faces
   `(font-lock-type-face ((t (:foreground "#bf3f1e"))))
   `(mode-line-inactive ((t (:background ,(face-attribute 'mode-line :background))))))
  (custom-theme-set-faces
   'twilight-anti-bright
   `(auto-dim-other-buffers-face
     ((t :foreground ,(color-from 'default :foreground -10)
         :background ,(color-from 'default :background +1))))
   `(clojure-if-true-face
     ((t (:background ,(-> 'font-lock-keyword-face
                           (face-attribute :background)
                           (dim-color 4.35)
                           (saturate-color -8))))))
   `(clojure-meta-face ((t (:foreground "#b0b0b0"))))
   `(cursor ((t (:background "deep sky blue"))))
   `(cperl-nonoverridable-face ((t (:inherit font-lock-constant-face :foreground ,(color-from 'font-lock-constant-face :foreground +10)))))
   `(evil-ex-lazy-highlight ((t (:inherit (bold lazy-highlight)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-regexp-grouping-construct))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,(color-from 'font-lock-string-face :foreground -5)))))
   `(fringe ((t (:background ,(color-from 'default :background -2)))))
   `(git-gutter+-added    ((t (:foreground ,(face-attribute 'diff-refine-added   :background)))))
   `(git-gutter+-deleted  ((t (:foreground ,(face-attribute 'diff-refine-removed :background)))))
   `(git-gutter+-modified ((t (:foreground ,(face-attribute 'diff-refine-changed :background)))))
   `(git-timemachine-minibuffer-detail-face ((t (:foreground nil :inherit highlight))))
   `(lazy-highlight ((t :background "paleturquoise4" :foreground "paleturquoise3")))
   `(linum ((t (:background "#000000" :foreground "#666666"))))
   `(linum-relative-current-face ((t (:inherit linum :foreground "#aaaaaa"))))
   `(org-cancelled ((t (:foreground nil :inherit org-done))))
   `(org-column ((t (:inherit bold))))
   `(org-block
     ((t :foreground ,(color-from 'default :foreground -15)
         :background ,(color-from 'default :background -5))))
   `(org-hide ((t (:foreground ,(face-attribute 'default :background) :background unspecified))))
   `(org-link ((t (:inherit link))))
   `(org-next ((t (:foreground "#dca3a3" :inherit (bold org-todo)))))
   `(region ((t (:background ,(color-from 'region :background +10)))))
   `(shadow ((t (:foreground "#9a9a9a"))))
   `(trailing-whitespace ((t (:background "gray35"))))))

(use-package twilight-bright-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'twilight-bright t)
  (custom-set-faces
   `(link ((t (:inherit underline :weight normal))))
   `(linum-relative-current-face ((t (:inherit linum))))
   `(magit-section-heading ((t (:inherit bold :foreground "DarkGoldenrod4"))))
   `(magit-diff-file-heading ((t (:weight medium))))
   `(magit-commit-log-type-face  ((t (:inherit font-lock-function-name-face :weight medium))))
   `(magit-commit-log-scope-face ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(show-paren-match ((t (:inherit underline :foreground "Cyan2" :background nil)))))
  (custom-theme-set-faces
   'twilight-bright
   `(auto-dim-other-buffers-face
     ((t :foreground ,(color-from 'default :foreground +4)
         :background ,(color-from 'default :background -2))))
   `(diff-refine-changed ((t (:weight bold :background ,(face-attribute 'diff-refine-changed :background)))))
   `(clojure-if-true-face
     ((t (:background ,(color-from 'font-lock-keyword-face :background +2.5)))))
   `(cursor ((t (:background "sky blue"))))
   `(evil-ex-lazy-highlight ((t (:inherit (bold lazy-highlight)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-regexp-grouping-construct))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,(-> 'font-lock-string-face
                                                                             (face-attribute :foreground)
                                                                             (light-color 5)
                                                                             (saturate-color 10))))))
   `(fringe ((t (:background ,(color-from 'default :background -1)))))
   `(git-gutter+-added    ((t (:foreground ,(face-attribute 'diff-refine-added   :background)))))
   `(git-gutter+-deleted  ((t (:foreground ,(face-attribute 'diff-refine-removed :background)))))
   `(git-gutter+-modified ((t (:foreground ,(face-attribute 'diff-refine-changed :background)))))
   `(git-timemachine-minibuffer-detail-face ((t (:foreground nil :inherit highlight))))
   `(hl-line ((t (:background "#eef7fd"))))
   `(linum-relative-current-face ((t (:inherit linum :foreground ,(color-from 'default :foreground +15)))))
   `(link ((t (:foreground "#55850f" :underline t))))
   `(magit-diff-context-highlight ((t (:background "#f2f9fd"))))
   `(magit-diff-hunk-heading-highlight ((t (:background "#c8e9ff"))))
   `(magit-section-highlight ((t (:background "#eef7fd"))))
   `(nlinum-relative-current-face ((t (:inherit linum-relative-current-face))))
   `(org-cancelled ((t (:foreground nil :inherit org-done))))
   `(org-column ((t (:inherit bold))))
   `(org-hide ((t (:foreground ,(face-attribute 'default :background) :background unspecified))))
   `(org-link ((t (:inherit link))))
   `(org-next ((t (:foreground "#dca3a3" :inherit (bold org-todo)))))
   `(outline-4 ((t (:inherit font-lock-string-face))))
   `(trailing-whitespace ((t (:background "gray65"))))))

(use-package plan9-theme
  :ensure t
  :config
  (custom-set-faces
   `(show-paren-match ((t (:foreground "Blue4")))))
  (custom-theme-set-faces
   'plan9
   `(diff-refine-added   ((t (:inherit diff-added   :weight bold :background ,(color-from 'diff-refine-added   :background -10)))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold :background ,(color-from 'diff-refine-removed :background -10)))))
   `(elixir-atom-face ((t (:inherit font-lock-builtin-face))))
   `(evil-goggles-yank-face ((t (:inherit evil-goggles-default-face))))
   `(font-lock-builtin-face       ((t (:inherit default :weight bold :foreground ,(color-from 'default :foreground +13)))))
   `(font-lock-function-name-face ((t (:inherit default :weight bold :foreground ,(color-from 'default :foreground +5)))))))

(use-package goto-addr
  :defer t
  :config
  (setq goto-address-mail-face "link"))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t (:inherit default :underline nil :height 1.0 :distant-foreground ,(face-attribute 'linum :foreground)))))))

(use-package magit
  :defer t
  :config
  (dolist (face '(magit-branch-current
                  magit-branch-local
                  magit-branch-remote
                  magit-head
                  magit-tag))
    (set-face-attribute face nil :inherit
                        (let ((inherit (face-attribute face :inherit)))
                          (if (listp inherit)
                              (cons 'bold inherit)
                            (list 'bold inherit))))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-colors
        (--iterate (dim-color it 10)
                   (apply 'color-rgb-to-hex (color-name-to-rgb "Springgreen"))
                   4)))

(use-package org
  :after plan9-theme
  :config
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i))))
          (outline-face (intern (concat "outline-" (number-to-string i)))))
      (set-face-attribute face nil
                          :background 'unspecified
                          :box nil
                          :height (if (<= i 1)
                                      1.1
                                    1.0)
                          :overline nil
                          :underline nil
                          :inherit outline-face))))

(use-package rainbow-delimiters
  :after plan9-theme
  :config
  (dolist (i (number-sequence 1 9))
    (let ((face (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))))
      (set-face-attribute face nil :foreground
                          (-> face
                              (face-attribute :foreground)
                              (saturate-color -10))))))

(use-package outline
  :defer t
  :config
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "outline-" (number-to-string i)))))
      (set-face-attribute face nil :inherit
                          (let ((inherit (face-attribute face :inherit)))
                            (if (listp inherit)
                                (cons 'bold inherit)
                              (list 'bold inherit)))))))

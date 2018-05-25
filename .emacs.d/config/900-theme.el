(let ((default-family (face-attribute 'default :family))
      (mode-line-box-face '(:line-width 2 :color "grey75" :style released-button)))
  (custom-set-faces
   `(bold ((t (:weight bold :family "MonacoB2"))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
   `(clojure-fn-parameter-face ((t (:inherit font-lock-variable-name-face :family ,default-family))))
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(css-property ((t (:inherit font-lock-builtin-face :foreground nil :family ,default-family))))
   `(css-selector ((t (:inherit (bold font-lock-variable-name-face) :foreground nil))))
   `(font-lock-comment-face ((t (:slant unspecified))))
   `(font-lock-doc-face ((t (:slant italic))))
   `(font-lock-function-name-face ((t (:inherit bold :weight unspecified))))
   `(font-lock-keyword-face ((t (:inherit nil :weight unspecified))))
   `(font-lock-type-face    ((t (:inherit nil :weight unspecified))))
   `(font-lock-variable-name-face ((t (:inherit bold :weight unspecified))))
   `(hl-line ((t (:inverse-video nil))))
   `(lisp-local-binding-variable-name-face ((t (:inherit font-lock-variable-name-face :family ,default-family))))
   `(link ((t (:inherit underline :weight unspecified))))
   `(linum ((t (:inherit default :underline nil :height 1.0 :distant-foreground ,(face-attribute 'linum :foreground)))))
   `(linum-relative-current-face ((t (:inherit linum))))
   `(magit-section-heading ((t (:inherit bold :foreground "DarkGoldenrod4"))))
   `(magit-commit-log-type-face  ((t (:inherit font-lock-function-name-face :family ,default-family))))
   `(magit-commit-log-scope-face ((t (:inherit font-lock-variable-name-face :family ,default-family))))
   `(mode-line ((t (:distant-foreground ,(face-attribute 'mode-line :foreground) :box ,mode-line-box-face))))
   `(mode-line-inactive ((t (:distant-foreground ,(face-attribute 'mode-line-inactive :foreground) :box ,mode-line-box-face))))
   `(nlinum-current-line ((t (:inherit linum))))
   `(nlinum-relative-current-face ((t (:inherit linum))))
   `(org-agenda-date-today ((t (:inherit (bold org-agenda-date)))))
   `(org-mode-line-clock ((t)))
   `(show-paren-match ((t (:inherit underline :foreground "Cyan2" :background nil))))
   `(widget-button ((t (:inherit bold))))))

(use-package twilight-bright-theme
  :ensure t
  :config
  (load-theme 'twilight-bright t)
  (custom-theme-set-faces
   'twilight-bright
   `(auto-dim-other-buffers-face
     ((t :foreground  ,(-> 'default (face-attribute :foreground) (light-color 5))
         :background  ,(-> 'default (face-attribute :background) (dim-color 3)))))
   `(clojure-if-true-face
     ((t (:background ,(-> 'font-lock-keyword-face
                           (face-attribute :background)
                           (light-color 2.5))))))
   `(cursor ((t (:background "sky blue"))))
   `(evil-ex-lazy-highlight ((t (:inherit (bold lazy-highlight)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-regexp-grouping-construct))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,(-> 'font-lock-string-face
                                                                             (face-attribute :foreground)
                                                                             (light-color 5)
                                                                             (saturate-color 10))))))
   `(fringe ((t (:background ,(-> 'default (face-attribute :background) (dim-color 1))))))
   `(git-gutter+-added    ((t (:foreground ,(face-attribute 'diff-refine-added   :background)))))
   `(git-gutter+-deleted  ((t (:foreground ,(face-attribute 'diff-refine-removed :background)))))
   `(git-gutter+-modified ((t (:foreground ,(face-attribute 'diff-refine-changed :background)))))
   `(git-timemachine-minibuffer-detail-face ((t (:foreground nil :inherit highlight))))
   `(hl-line ((t (:background "#eef7fd"))))
   `(linum-relative-current-face ((t (:inherit linum :foreground ,(-> 'default
                                                                      (face-attribute :foreground)
                                                                      (light-color 15))))))
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
   `(powerline-active1   ((t (:foreground "#85CEEB" :background "#383838" :inherit mode-line))))
   `(powerline-active2   ((t (:foreground "#85CEEB" :background "#6b6b6b" :inherit mode-line))))
   `(powerline-inactive1 ((t (:foreground "#F0F0EF" :background "#686868" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:foreground "#F0F0EF" :background "#A9A9A9" :inherit mode-line-inactive))))
   `(trailing-whitespace ((t (:background "gray65"))))))

(use-package goto-addr
  :defer t
  :config
  (setq goto-address-mail-face "link"))

(use-package magit
  :defer t
  :config
  (dolist (face '(magit-branch-current magit-branch-local magit-branch-remote magit-head magit-tag))
    (set-face-attribute face nil :inherit
                        (let ((inherit (face-attribute face :inherit)))
                          (if (listp inherit)
                              (cons 'bold inherit)
                            (list 'bold inherit))))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-colors (--iterate (dim-color it 10)
                                   (apply 'color-rgb-to-hex (color-name-to-rgb "Springgreen"))
                                   4)))

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

(custom-set-faces
 `(default ((t (:weight medium :height ,font-height))))
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
 `(widget-button ((t (:inherit bold)))))

(use-package twilight-anti-bright-theme
  :ensure t
  :config
  (load-theme 'twilight-anti-bright t)
  (custom-set-faces
   `(default ((t (:foreground "#CBCCCC"))))
   `(font-lock-type-face ((t (:foreground "#bf3f1e"))))
   `(font-lock-comment-face ((t (:inherit italic :background unspecified))))
   `(link ((t (:inherit underline :weight normal))))
   `(linum-relative-current-face ((t (:inherit linum))))
   `(magit-section-heading ((t (:inherit bold :foreground "DarkGoldenrod4"))))
   `(magit-diff-file-heading ((t (:weight medium))))
   `(magit-commit-log-type-face  ((t (:inherit font-lock-function-name-face :weight medium))))
   `(magit-commit-log-scope-face ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(show-paren-match ((t (:inherit underline :foreground "Cyan2" :background nil))))
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
   `(region ((t (:background ,(color-from 'region :background +10)))))
   `(shadow ((t (:foreground "#9a9a9a"))))
   `(trailing-whitespace ((t (:background "gray35"))))))

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
  (custom-set-faces
   `(hl-paren-face ((t (:weight bold)))))
  (setq hl-paren-colors
        (--iterate (dim-color it 10)
                   (apply 'color-rgb-to-hex (color-name-to-rgb "Springgreen"))
                   4)))

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

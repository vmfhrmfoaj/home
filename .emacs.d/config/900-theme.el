(use-package twilight-anti-bright-theme
  :ensure t
  :config
  (custom-theme-set-faces
   'twilight-anti-bright
   `(auto-dim-other-buffers-face
     ((t :foreground ,(color-from 'default :foreground -10)
         :background ,(color-from 'default :background +1))))
   `(cursor ((t :background "deep sky blue")))
   `(evil-ex-lazy-highlight ((t :inherit (bold lazy-highlight))))
   `(font-lock-regexp-grouping-backslash ((t :inherit font-lock-regexp-grouping-construct)))
   `(font-lock-regexp-grouping-construct ((t :inherit bold :foreground ,(color-from 'font-lock-string-face :foreground -5))))
   `(fringe ((t :background ,(color-from 'default :background -3))))
   `(lazy-highlight ((t :background "paleturquoise4" :foreground "paleturquoise3")))
   `(region ((t :background ,(color-from 'region :background +10))))
   `(shadow ((t :foreground "#9a9a9a")))
   `(trailing-whitespace ((t :background "gray35"))))
  (custom-set-faces
   `(font-lock-comment-face ((t :background unspecified)))
   `(font-lock-doc-face ((t :slant italic)))
   `(font-lock-function-name-face ((t :inherit bold)))
   `(font-lock-string-face ((t :slant normal)))
   `(font-lock-type-face ((t :foreground "#bf3f1e")))
   `(font-lock-warning-face ((t :inherit bold)))
   `(hl-line ((t :inverse-video nil)))
   `(link ((t :inherit underline :weight normal)))
   `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face)))
   `(mode-line ((t :weight normal)))
   `(mode-line-inactive ((t :background ,(face-attribute 'mode-line :background))))
   `(mode-line-inactive ((t :inherit mode-line)))
   `(show-paren-match ((t :inherit (bold underline) :foreground "Cyan2" :background nil)))
   `(widget-button ((t :inherit bold)))))

(use-package cperl-mode
  :defer t
  :config
  (custom-set-faces
   `(cperl-nonoverridable-face ((t :inherit font-lock-constant-face :foreground ,(color-from 'font-lock-constant-face :foreground +10))))))

(use-package cider-mode
  :defer t
  :config
  (custom-set-faces
   `(cider-fringe-good-face ((t :inherit success)))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   `(clojure-if-true-face
     ((t :background ,(-> 'font-lock-keyword-face
                          (face-attribute :background)
                          (dim-color 4.35)
                          (saturate-color -8)))))
   `(clojure-meta-face ((t :foreground "#b0b0b0")))
   `(clojure-define-type-face   ((t :inherit (bold font-lock-type-face))))
   `(clojure-defining-spec-face ((t :inherit (bold clojure-keyword-face))))
   `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight normal)))
   `(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   `(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   `(clojure-side-effect-face ((t :inherit (bold italic font-lock-warning-face))))
   `(clojure-special-variable-name-face ((t :inherit clojure-fn-parameter-face)))))

(use-package css-mode
  :defer t
  :config
  (custom-set-faces
   `(css-property ((t :inherit font-lock-builtin-face :foreground nil :weight normal)))
   `(css-selector ((t :inherit (bold font-lock-variable-name-face) :foreground nil)))))

(use-package git-gutter+
  :defer t
  :config
  (custom-set-faces
   `(git-gutter+-added    ((t :foreground ,(face-attribute 'diff-refine-added   :background))))
   `(git-gutter+-deleted  ((t :foreground ,(face-attribute 'diff-refine-removed :background))))
   `(git-gutter+-modified ((t :foreground ,(face-attribute 'diff-refine-changed :background))))))

(use-package git-timemachine
  :defer t
  :config
  (custom-set-faces
   `(git-timemachine-minibuffer-detail-face ((t :foreground nil :inherit highlight)))))

(use-package goto-addr
  :defer t
  :config
  (setq goto-address-mail-face "link"))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch)))
   '(helm-selection-line ((t :foreground unspecified)))))

(use-package highlight-parentheses
  :defer t
  :config
  (custom-set-faces
   `(hl-paren-face ((t :inherit bold))))
  (setq hl-paren-colors
        (--iterate (dim-color it 10)
                   (apply 'color-rgb-to-hex (color-name-to-rgb "Springgreen"))
                   4)))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t :inherit default :underline nil :height 1.0 :distant-foreground ,(face-attribute 'linum :foreground))))
   `(linum-relative-current-face ((t :inherit linum)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   `(lsp-face-highlight-read  ((t :inherit  highlight       :underline t)))
   `(lsp-face-highlight-write ((t :inherit (highlight bold) :underline (:color foreground-color :style wave))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   `(magit-section-heading ((t :inherit bold :foreground "DarkGoldenrod4")))
   `(magit-diff-file-heading ((t :weight normal)))
   `(magit-commit-log-type-face  ((t :inherit font-lock-function-name-face :weight normal)))
   `(magit-commit-log-scope-face ((t :inherit font-lock-variable-name-face :weight normal))))
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

(use-package rust-mode
  :defer t
  :config
  (custom-set-faces
   `(rust-question-mark-face ((t :inherit font-lock-builtin-face :weight unspecified)))))


;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package base16-theme
  :ensure t
  :config
  (setq frame-background-mode 'dark)
  (load-theme 'base16-tomorrow-night t)

  (custom-set-faces
   '(highlight ((t :background "#484a4e" :foreground "#d5d8d6")))
   '(shadow ((t :weight normal)))))

(use-package company
  :defer t
  :config
  (custom-set-faces
   '(company-preview ((t :inherit shadow :underline t)))
   '(company-preview-common ((t :inherit company-preview)))))

(use-package clojure-mode
  :defer t
  :config
  (let ((default-weight (face-attribute 'default :weight)))
    (custom-set-faces
     `(cider-deprecated-face
       ((t :inherit font-lock-warning-face :weight ,default-weight :underline (:color "darkorange"))))
     '(cider-fringe-good-face ((t :inherit success)))
     '(clojure-cond-condtion-face ((t :slant italic)))
     '(clojure-define-type-face ((t :inherit font-lock-type-face)))
     '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
     `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,default-weight)))
     '(clojure-fn-parameter-unused-face ((t :inherit shadow)))
     '(clojure-meta-face ((t :inherit shadow :weight extra-light)))
     '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
     '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
     '(clojure-side-effect-face ((t :weight normal :underline t)))
     '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))
     '(clojure-special-variable-definition-face
       ((t :inherit (font-lock-constant-face clojure-variable-definition-face))))
     '(clojure-variable-definition-face ((t :inherit font-lock-variable-name-face :weight normal))))))

(use-package cperl-mode
  :defer t
  :config
  (custom-set-faces
   '(cperl-nonoverridable-face ((t :inherit font-lock-constant-face)))))

(use-package eldoc
  :defer t
  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :weight semi-bold :underline t)))))

(use-package evil-goggles
  :defer t
  :config
  (let ((common-style
         '(:underline unspecified :slant unspecified :overline unspecified :box unspecified :inverse-video t)))
    (custom-set-faces
     `(evil-goggles-delete-face
       ((((class color) (background light)) :background "#eecccc" ,@common-style)
        (((class color) (background dark))  :inherit diff-removed ,@common-style)))
     `(evil-goggles-change-face
       ((((class color) (background light)) :background "#eecccc" ,@common-style)
        (((class color) (background dark))  :inherit diff-removed ,@common-style)))
     `(evil-goggles-paste-face
       ((((class color) (background light)) :background "#cceecc" ,@common-style)
        (((class color) (background dark))  :inherit diff-added   ,@common-style)))
     `(evil-goggles-yank-face
       ((((class color) (background light)) :background "#eeeecc" ,@common-style)
        (((class color) (background dark))  :inherit diff-changed ,@common-style)))
     '(evil-goggles-undo-redo-remove-face ((t :inherit evil-goggles-delete-face)))
     '(evil-goggles-undo-redo-add-face    ((t :inherit evil-goggles-paste-face)))
     '(evil-goggles-undo-redo-change-face ((t :inherit evil-goggles-change-face))))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :weight normal)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :foreground "#787978" :weight normal)))
   '(font-lock-doc-face ((t :weight normal)))))

(use-package ivy
  :defer t
  :config
  (custom-set-faces
   '(ivy-current-match ((((class color) (background light)) :background "#9fcdf2" :foreground "white")
                        (((class color) (background dark))  :background "#5f6369" :foreground "white")))
   '(ivy-grep-info ((((class color) (background dark)) :inherit font-lock-string-face)))
   '(ivy-minibuffer-match-face-1
     ((((class color) (background light)) :background "#d3d3d3" :foreground "#8a8a8a")
      (((class color) (background dark))  :background "#7d7e7f" :foreground "#535454")))
   '(ivy-minibuffer-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b")))
   '(ivy-minibuffer-match-face-3 ((t :background "#bbbbff" :foreground "#707099")))
   '(ivy-minibuffer-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f")))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context ((t  :inherit magit-diff-context-highlight :weight normal)))))

(use-package org
  :defer t
  :config
  (custom-set-faces
   '(org-agenda-date ((t :foreground "dark cyan" :height 1.1)))
   '(org-agenda-date-weekend ((t :inherit org-agenda-date)))
   '(org-agenda-date-today ((t :inherit org-agenda-date :foreground "turquoise")))
   '(org-date ((t :slant italic :underline unspecified)))
   '(org-done ((t :box t)))
   '(org-drawer ((t :foreground "light sky blue" :weight normal)))
   `(org-hide ((t :inherit default :background unspecified :foreground ,(bg-color-from 'default))))
   '(org-link ((t :inherit link :underline unspecified)))
   '(org-property-value ((t :weight normal)))
   '(org-special-keyword ((t :weight normal)))
   '(org-tag ((t :weight normal)))
   '(org-todo ((t :box t)))
   '(org-warning ((t :inherit font-lock-warning-face :underline nil))))
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :height 1.1))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

(use-package rpm-spec-mode
  :defer t
  :config
  (custom-set-faces
   '(rpm-spec-ghost-face ((t :inherit shadow)))
   '(rpm-spec-macro-face ((t :inherit font-lock-keyword-face)))
   '(rpm-spec-package-face ((t :inherit font-lock-constant-face)))
   '(rpm-spec-section-face ((t :inherit font-lock-function-name-face)))
   '(rpm-spec-tag-face ((t :inherit font-lock-builtin-face)))
   '(rpm-spec-var-face ((t :inherit font-lock-variable-name-face)))))

(use-package rust-mode
  :defer t
  :config
  (custom-set-faces
   '(rust-attribute-face ((t :inherit font-lock-preprocessor-face :weight normal)))))

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc
     ((((class color) (background light)) :background "#fff6e5" :foreground "orange2")
      (((class color) (background dark))  :background "#201818" :foreground "tan1")))
   `(sh-quoted-exec
     ((((class color) (background light)) :background "#faecfa" :foreground "magenta")
      (((class color) (background light)) :background "#191719" :foreground "magenta")))))

(use-package swiper
  :defer t
  :config
  (custom-set-faces
   '(swiper-line-face
     ((((class color) (background light)) :background "#9fcdf2" :foreground "white")
      (((class color) (background dark))  :background "#5f6369" :foreground "white")))
   '(swiper-match-face-1
     ((((class color) (background light)) :inherit swiper-line-face :background "#d3d3d3")
      (((class color) (background dark))  :inherit swiper-line-face :background "#7d7e7f")))
   '(swiper-match-face-2 ((t :inherit swiper-line-face :background "#e99ce8")))
   '(swiper-match-face-3 ((t :inherit swiper-line-face :background "#bbbbff")))
   '(swiper-match-face-4 ((t :inherit swiper-line-face :background "#ffbbff")))
   '(swiper-background-match-face-1
     ((((class color) (background light)) :background "#d3d3d3" :foreground "#8a8a8a")
      (((class color) (background dark))  :background "#7d7e7f" :foreground "#535454")))
   '(swiper-background-match-face-2 ((t :background "#e99ce8" :foreground "#8b5d8b")))
   '(swiper-background-match-face-3 ((t :background "#bbbbff" :foreground "#707099")))
   '(swiper-background-match-face-4 ((t :background "#ffbbff" :foreground "#7f5d7f"))))
  ;; copy from `swiper--recompute-background-faces'
  (let ((faces '(swiper-match-face-1
                 swiper-match-face-2
                 swiper-match-face-3
                 swiper-match-face-4))
        (colir-compose-method #'colir-compose-alpha)
        (line-bg (face-background 'swiper-line-face)))
    (cl-mapc (lambda (f1 f2)
               (let ((bg (face-background f1)))
                 (when bg
                   (set-face-background
                    f2
                    (colir-blend
                     (colir-color-parse bg)
                     (colir-color-parse line-bg))))))
             swiper-faces
             faces)))

(use-package treemacs
  :defer t
  :config
  (custom-set-faces
   `(treemacs-fringe-indicator-face ((t :foreground ,(face-background 'cursor))))))

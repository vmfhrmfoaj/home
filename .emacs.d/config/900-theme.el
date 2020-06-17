(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (setq frame-background-mode 'dark)
  (load-theme 'sanityinc-tomorrow-night t))

(use-package font-lock
  :config
  (defvar local-variable-name-light-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground
                                (if (eq frame-background-mode 'light) 7 -7))
                    (if (eq frame-background-mode 'light) -10 -20))
    "A foreground color for the local variable such as parameters"))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((((background dark))
       :foreground ,(color-from 'default :foreground -10)
       :background ,(color-from 'default :background +2))))))

(use-package clojure-mode
  :disabled t
  :defer t
  :config
  (custom-set-faces
   '(cider-fringe-good-face ((t :inherit success)))
   '(clojure-define-type-face ((t :inherit font-lock-type-face)))
   '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
   `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face
                                   :foreground ,local-variable-name-light-fg-color
                                   :weight ,(face-attribute 'default :weight))))
   '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   '(clojure-side-effect-face ((t :inherit (bold italic font-lock-warning-face))))
   '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))))

(use-package elisp-mode
  :defer t
  :config
  (custom-set-faces
   `(lisp-local-binding-variable-name-face ((t :inherit font-lock-variable-name-face
                                               :foreground ,local-variable-name-light-fg-color
                                               :weight ,(face-attribute 'default :weight))))))

(use-package evil-goggles
  :defer t
  :config
  (custom-set-faces
   '(evil-goggles-delete-face ((t (:inherit diff-refine-removed :inverse-video t))))
   '(evil-goggles-change-face ((t (:inherit diff-refine-removed :inverse-video t))))
   '(evil-goggles-paste-face  ((t (:inherit diff-refine-added   :inverse-video t))))
   '(evil-goggles-yank-face   ((t (:inherit diff-refine-changed :inverse-video t))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed :inverse-video t))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-refine-added   :inverse-video t))))
   '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed :inverse-video t))))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch :weight bold)))
   '(helm-selection-line ((t :inherit helm-selection :weight bold)))))

(use-package hl-line
  :defer t
  :config
  (custom-set-faces
   '(hl-line ((t :inherit highlight :weight bold)))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit highlight :underline t)))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit highlight :underline t)))
   '(lsp-face-highlight-textual ((t :inherit highlight :underline t)))
   '(lsp-face-highlight-write   ((t :inherit highlight :underline t :weight bold)))))

(use-package base16-theme
  :ensure t
  :config
  (setq frame-background-mode 'dark
        x-underline-at-descent-line t)
  (load-theme 'base16-tomorrow-night t)

  (custom-set-faces
   '(shadow ((t :foreground "#a6a9a7")))
   '(fixed-pitch ((t :family "Dejavu Sans Mono")))))

(use-package font-lock
  :config
  (custom-set-faces
   '(font-lock-comment-delimiter-face ((t :foreground "#5a5b5a")))
   '(font-lock-doc-face ((t :foreground "#b1b3b1"))))

  (defvar local-variable-name-light-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground
                                (if (eq frame-background-mode 'light) 7 -7))
                    (if (eq frame-background-mode 'light) -10 -20))
    "A foreground color for the local variable such as parameters"))

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

(use-package diff-mode
  :defer t
  :config
  (custom-set-faces
   '(diff-added   ((t :foreground "#b5bd68")))
   '(diff-removed ((t :foreground "#de935f")))
   '(diff-changed ((t :foreground "#81a2be")))
   '(diff-refine-added   ((t :foreground "#8abeb7" :underline t)))
   '(diff-refine-removed ((t :foreground "#cc6666" :underline t)))
   '(diff-refine-changed ((t :foreground "#f0c674" :underline t)))))

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
  (let ((common-style '(:underline unspecified :slant unspecified :overline unspecified :box unspecified :inverse-video t)))
    (custom-set-faces
     `(evil-goggles-delete-face ((t :inherit diff-refine-removed ,@common-style)))
     `(evil-goggles-change-face ((t :inherit diff-refine-removed ,@common-style)))
     `(evil-goggles-paste-face  ((t :inherit diff-refine-added   ,@common-style)))
     `(evil-goggles-yank-face   ((t :inherit diff-refine-changed ,@common-style)))
     '(evil-goggles-undo-redo-remove-face ((t :inherit evil-goggles-delete-face)))
     '(evil-goggles-undo-redo-add-face    ((t :inherit evil-goggles-paste-face)))
     '(evil-goggles-undo-redo-change-face ((t :inherit evil-goggles-change-face))))))

(use-package flymake
  :defer t
  :config
  (custom-set-faces
   `(flymake-warnline ((t :underline (:style wave :color ,(color-from 'flymake-warnline :underline)))))
   `(flymake-errline  ((t :underline (:style wave :color ,(color-from 'flymake-errline  :underline)))))
   `(flymake-warning  ((t :underline (:style wave :color ,(color-from 'flymake-warning  :underline)))))
   `(flymake-error    ((t :underline (:style wave :color ,(color-from 'flymake-error    :underline)))))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-ff-file-dir       ((t :inherit helm-ff-file :foreground "#608580")))
   '(helm-ff-file-extension ((t :inherit helm-ff-file)))
   '(helm-ff-executable-dir ((t :inherit helm-ff-executable :foreground "#909753")))
   '(helm-ff-executable-file-extension ((t :inherit helm-ff-executable)))
   '(helm-match ((t :inherit lazy-highlight :foreground unspecified)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch :weight bold)))
   '(helm-selection ((t :inherit highlight :weight bold)))
   '(helm-selection-line ((t :inherit helm-selection :weight bold)))
   `(helm-other-buffer ((((background dark))
                         :foreground ,(color-from 'default :foreground -10)
                         :background ,(color-from 'default :background +2))))))

(use-package hl-line
  :defer t
  :config
  (custom-set-faces
   '(hl-line ((t :inherit highlight)))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit highlight :underline t)))))

(use-package isearch
  :defer t
  :config
  (custom-set-faces
   '(lazy-highlight ((t :weight bold)))))

(use-package display-line-numbers
  :defer t
  :config
  (custom-set-faces
   '(line-number ((t :background "#222427" :foreground "#787978")))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context           ((t :foreground "#969896")))
   '(magit-diff-context-highlight ((t :background "#282a2e" :foreground "#969896")))
   '(magit-diff-hunk-heading           ((t :background "#373b41" :foreground "#c5c8c6")))
   '(magit-diff-hunk-heading-highlight ((t :background "#373b41")))
   '(magit-diff-file-heading ((t :weight normal)))
   '(magit-diff-added   ((t :inherit diff-added)))
   '(magit-diff-removed ((t :inherit diff-removed)))
   '(magit-diff-added-highlight   ((t :inherit diff-added   :background "#282a2e")))
   '(magit-diff-removed-highlight ((t :inherit diff-removed :background "#282a2e")))
   '(magit-section-highlight ((t :weight bold)))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))))

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

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit highlight :underline t)))
   '(lsp-face-highlight-textual ((t :inherit highlight :underline t)))
   '(lsp-face-highlight-write   ((t :inherit highlight :underline t :weight bold)))
   '(lsp-diagnostic-level-1     ((t (:inherit compilation-error   :family "Noto Sans Mono" :weight light))))
   '(lsp-diagnostic-level-2     ((t (:inherit compilation-warning :family "Noto Sans Mono" :weight light))))
   '(lsp-diagnostic-level-3     ((t (:inherit compilation-info    :family "Noto Sans Mono" :weight light))))
   '(lsp-diagnostic-level-4     ((t (:inherit compilation-info    :family "Noto Sans Mono" :weight light))))))

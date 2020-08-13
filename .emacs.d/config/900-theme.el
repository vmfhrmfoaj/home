;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package base16-theme
  :ensure t
  :config
  (setq frame-background-mode 'dark
        x-underline-at-descent-line t)
  (load-theme 'base16-material-palenight t)

  (custom-set-faces
   '(fixed-pitch ((t :family "Dejavu Sans Mono")))
   `(region ((t :background ,(color-from 'default :background -2))))))

(use-package font-lock
  :config
  (custom-set-faces
   `(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :foreground ,(color-from 'font-lock-comment-face :foreground -10)))))

  (defvar local-variable-name-light-fg-color
    (color-from 'font-lock-variable-name-face :foreground
                (if (eq frame-background-mode 'light) 7 -10)
                (if (eq frame-background-mode 'light) -10 -30))
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
   `(diff-refine-added   ((t :foreground ,(color-from 'diff-added   :foreground 15) :underline t)))
   `(diff-refine-removed ((t :foreground ,(color-from 'diff-removed :foreground 10) :underline t)))
   `(diff-refine-changed ((t :foreground ,(color-from 'diff-changed :foreground 10) :underline t)))))

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
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight :foreground unspecified)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch :weight bold)))
   '(helm-selection ((t :inherit hl-line :weight bold)))
   '(helm-selection-line ((t :inherit helm-selection :weight bold)))
   `(helm-other-buffer ((t :foreground ,(color-from 'default :foreground -10 -15)
                           :background ,(color-from 'default :background +2))))))

(use-package helm-files
  :defer t
  :config
 (custom-set-faces
   `(helm-ff-file-dir       ((t :inherit helm-ff-file :foreground ,(color-from 'helm-ff-file :foreground -15 -40))))
   '(helm-ff-file-extension ((t :inherit font-lock-constant-face)))
   `(helm-ff-executable-dir ((t :inherit helm-ff-executable :foreground ,(color-from 'helm-ff-executable :foreground -15 -40))))))

(use-package hl-line
  :defer t
  :init
  (defface hl-line-evil-insert
    '((t (:weight unspecified)))
    "TODO")

  :config
  (custom-set-faces
   '(hl-line ((t :inherit hl-line-evil-insert)))))

(use-package highlight-parentheses
  :defer t
  :config
  (let ((base-color "#82aaff"))
    (setq hl-paren-colors
          (--iterate (saturate-color (dim-color it 10) -20)
                     (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb base-color))
                     4))))

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
   '(line-number ((t :background "#363452")))
   '(line-number-current-line ((t :inherit (hl-line line-number) :foreground "#959dcb" :inverse-video unspecified)))))

(use-package magit
  :defer t
  :config
  (let ((highlight-bg (color-from 'hl-line :background -5)))
    (custom-set-faces
     `(magit-diff-context-highlight ((t :background ,highlight-bg)))
     `(magit-diff-hunk-heading-highlight ((t :background ,highlight-bg)))
     '(magit-diff-file-heading ((t :weight normal)))
     '(magit-diff-added   ((t :inherit diff-added)))
     '(magit-diff-removed ((t :inherit diff-removed)))
     `(magit-diff-added-highlight   ((t :inherit diff-added   :background ,highlight-bg)))
     `(magit-diff-removed-highlight ((t :inherit diff-removed :background ,highlight-bg)))
     '(magit-section-highlight ((t :inherit hl-line))))))

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

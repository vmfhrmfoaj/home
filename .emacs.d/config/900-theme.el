;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package twilight-bright-theme
  :ensure t
  :config
  (defface semi-bold
    '((t (:weight semi-bold)))
    "TODO")

  (load-theme 'twilight-bright t)
  (custom-set-faces
   '(bold ((t :weight bold)))
   '(italic ((t :slant italic)))
   '(cursor ((t :background "sky blue")))
   '(diff-added   ((t :background "#ddffdd" :foreground "#22aa22")))
   '(diff-changed ((t :background "#ffffdd" :foreground "#aaaa22")))
   '(diff-removed ((t :background "#ffdddd" :foreground "#aa2222")))
   '(diff-refine-added   ((t :inherit bold :background "#eeffee" :foreground "#008800")))
   '(diff-refine-changed ((t :inherit bold :background "#ffffee" :foreground "#888800")))
   '(diff-refine-removed ((t :inherit bold :background "#ffeeee" :foreground "#880000")))
   '(line-number ((t :inherit (default) :background "grey99" :foreground "grey80")))
   '(line-number-current-line ((t :inherit (hl-line line-number) :background "#f1f8fd" :foreground "grey75")))
   '(fixed-pitch ((t :family "Dejavu Sans Mono")))
   '(link ((t :underline t)))
   '(shadow ((t :inherit default :foreground "grey55")))
   '(show-paren-match ((t :inherit bold :underline t)))
   '(trailing-whitespace ((t :background "grey65")))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   '(cider-fringe-good-face ((t :inherit success)))
   '(clojure-define-type-face ((t :inherit font-lock-type-face)))
   '(clojure-defining-spec-face ((t :inherit clojure-keyword-face)))
   `(clojure-fn-parameter-face ((t :inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
   '(clojure-important-keywords-face ((t :inherit (semi-bold font-lock-keyword-face))))
   '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   '(clojure-variable-name-face ((t :inherit (bold font-lock-variable-name-face))))
   '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   '(clojure-side-effect-face ((t :inherit (semi-bold underline))))
   '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))))

(use-package eldoc
  :defer t
  :config
  (custom-set-faces
   '(eldoc-highlight-function-argument ((t :inherit (semi-bold underline))))))

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

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused ((t :foreground "#a49da5")))))

(use-package font-lock
  :defer t
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :background unspecified :slant unspecified :weight normal)))
   '(font-lock-comment-delimiter-face ((t :inherit font-lock-comment-face :weight light)))
   '(font-lock-function-name-face ((t :inherit bold)))
   '(font-lock-regexp-grouping-backslash ((t :inherit font-lock-string-face :background "#E1F2D6")))
   '(font-lock-regexp-grouping-construct ((t :inherit font-lock-string-face :background "#E1F2D6")))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-variable-name-face ((t :foreground "#607596")))
   '(font-lock-warning-face ((t :inherit (semi-bold italic))))))

(use-package flymake
  :defer t
  :config
  (custom-set-faces
   '(flymake-warning ((t :underline (:style wave :color "gold"))))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   '(fringe ((t :background "grey98")))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-ff-file-dir       ((t :inherit helm-ff-file :foreground "#dbbd32")))
   '(helm-ff-file-extension ((t :inherit font-lock-constant-face)))
   '(helm-ff-executable     ((t :inherit helm-ff-executable :foreground "#5f9411")))
   '(helm-ff-executable-dir ((t :inherit helm-ff-executable :foreground "#7ea940")))
   '(helm-match ((t :inherit lazy-highlight :foreground unspecified)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit (bold isearch))))
   '(helm-moccur-buffer ((t :inherit shadow)))
   '(helm-selection ((t :inherit (bold hl-line))))
   '(helm-selection-line ((t :inherit (bold helm-selection))))
   `(helm-other-buffer ((t :background "grey98" :foreground ,(color-from 'default :foreground 5))))))

(use-package hl-line
  :defer t
  :init
  (defface hl-line-evil-insert
    '((t (:inherit semi-bold :background "#e2f0fb")))
    "TODO")

  :config
  (custom-set-faces
   '(hl-line ((t :background "#eef7fd")))))

(use-package hl-todo
  :defer t
  :config
  (custom-set-faces
   '(hl-todo ((t :inherit semi-bold :foreground "#cc9393")))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit (highlight underline semi-bold))))))

(use-package isearch
  :defer t
  :config
  (custom-set-faces
   '(isearch ((t :inherit semi-bold :background "magenta3" :foreground "white")))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((t :inherit font-lock-variable-name-face)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit (highlight underline))))
   '(lsp-face-highlight-textual ((t :inherit (highlight underline))))
   '(lsp-face-highlight-write   ((t :inherit (highlight underline semi-bold))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((t :background "#fbfeee" :foreground "#a4c207")))
   '(magit-diff-added   ((t :background "#ddffdd" :foreground "#22aa22")))
   '(magit-diff-removed ((t :background "#ffdddd" :foreground "#aa2222")))
   '(magit-diff-added-highlight   ((t :inherit semi-bold :background "#ddffdd" :foreground "#22aa22")))
   '(magit-diff-removed-highlight ((t :inherit semi-bold :background "#ffdddd" :foreground "#aa2222")))
   '(magit-diff-context ((t :foreground "grey50" :weight normal)))
   '(magit-diff-context-highlight ((t :inherit hl-line :foreground "grey50")))
   '(magit-diff-file-heading ((t :weight semi-bold)))
   '(magit-hash ((((class color) (background light)) :foreground "grey60" :weight normal)))
   '(magit-log-author ((((class color) (background light)) :foreground "firebrick" :weight normal)))
   '(magit-log-date ((((class color) (background light)) :foreground "grey30" :weight normal)))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc     ((t :background "#fcf7f2" :foreground "tan1")))
   `(sh-quoted-exec ((t :background "#faecfa" :foreground "magenta")))))

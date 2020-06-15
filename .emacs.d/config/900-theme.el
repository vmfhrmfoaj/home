(use-package twilight-bright-theme
  :ensure t
  :config
  (setq frame-background-mode 'light)
  (load-theme 'twilight-bright t))

(use-package twilight-anti-bright-theme
  :disabled t
  :ensure t
  :config
  (setq frame-background-mode 'dark)
  (load-theme 'twilight-anti-bright t))

(mapc #'frame-set-background-mode (frame-list))
(custom-set-faces
   '(bold ((t :weight bold)))
   '(cursor ((t :background "sky blue")))
   '(default ((((background dark)) :foreground "#cfd0d0")))
   `(fixed-pitch ((t :font ,alternative-font)))
   `(fixed-pitch-serif ((t :inherit fixed-pitch)))
   '(italic ((t :slant italic)))
   '(link ((t :underline t)))
   `(region ((t :distant-foreground ,(color-from 'default :foreground))))
   '(shadow ((((background light)) :inherit default :foreground "gray55")
             (((background dark))  :inherit default :foreground "#a5a6a6")))
   '(show-paren-match ((((background light)) :inherit bold :underline t)
                       (((background dark))  :inherit bold :underline t :foreground "firebrick1")))
   '(trailing-whitespace ((t :background "gray65"))))

(use-package font-lock
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :background unspecified :slant unspecified :weight light)))
   '(font-lock-function-name-face ((t :inherit bold)))
   '(font-lock-regexp-grouping-backslash ((((background light)) :inherit font-lock-string-face :background "#e1f2d6")))
   '(font-lock-regexp-grouping-construct ((((background light)) :inherit font-lock-string-face :background "#e1f2d6")))
   '(font-lock-type-face ((((background dark)) :foreground "#b63f1e") (t :weight unspecified)))
   '(font-lock-warning-face ((t :inherit (bold italic)))))

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
     ((((background light))
       :foreground ,(color-from 'default :foreground +4)
       :background ,(color-from 'default :background -2))
      (((background dark))
       :foreground ,(color-from 'default :foreground -5)
       :background ,(color-from 'default :background +1))))))

(use-package clojure-mode
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

(use-package focus
  :defer t
  :config
  (custom-set-faces
   `(focus-unfocused ((((background light)) :foreground "#a49da5")))))

(use-package flymake
  :defer t
  :config
  (custom-set-faces
   '(flymake-warning ((t :underline (:style wave :color "gold"))))))

(use-package fringe
  :defer t
  :config
  (custom-set-faces
   '(fringe ((((background light)) :background "gray98")))))

(use-package git-commit
  :defer t
  :config
  (custom-set-faces
   '(git-commit-summary ((t :inherit font-lock-function-name-face :weight normal)))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch)))
   '(helm-selection-line ((t :inherit helm-selection)))))


(use-package hl-line
  :defer t
  :config
  (custom-set-faces
   '(hl-line ((((background light)) :background "#eef7fd")))))

(use-package hl-todo
  :defer t
  :config
  (custom-set-faces
   '(hl-todo ((t :inherit bold :weight semi-bold :foreground "#cc9393")))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit (highlight underline bold))))))

(use-package isearch
  :defer t
  :config
  (custom-set-faces
   '(isearch ((((background light)) :inherit bold :background "magenta3" :foreground "white")))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   '(php-passive-assign-variable-face ((t :inherit font-lock-variable-name-face)))
   `(php-variable-name ((((class color) (background light))
                         :foreground ,(-> 'font-lock-variable-name-face
                                          (color-from :foreground 30)
                                          (saturate-color 10)
                                          (mix-color (color-from 'font-lock-string-face :foreground -30))))))))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   '(linum ((((class color) (background light) (min-colors 548)) :inherit (shadow default) :background "gray97")
            (((class color) (background light) (min-colors 256)) :inherit (shadow default) :background "color-255")
            (t :inherit default)))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   '(lsp-face-highlight-read    ((t :inherit (highlight underline))))
   '(lsp-face-highlight-textual ((t :inherit (highlight underline))))
   '(lsp-face-highlight-write   ((t :inherit (highlight underline bold))))))

(use-package lsp-ui
  :defer t
  :config
  (custom-set-faces
   `(lsp-diagnostic-level-1 ((((background light)) :inherit compilation-error :font ,alternative-font :weight light)
                             (((background dark)) :foreground "#8a2507" :font ,alternative-font)))
   `(lsp-diagnostic-level-2 ((((background light)) :inherit compilation-warning :font ,alternative-font :weight light)
                             (((background dark)) :foreground "#b26200" :font ,alternative-font)))
   `(lsp-diagnostic-level-3 ((((background light)) :inherit compilation-info :font ,alternative-font :weight light)
                             (((background dark)) :foreground "#007f00" :font ,alternative-font)))
   `(lsp-diagnostic-level-4 ((((background light)) :inherit compilation-info :font ,alternative-font :weight light)
                             (((background dark)) :foreground "#007f00" :font ,alternative-font)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((((class color) (background light)) :background "#fbfeee" :foreground "#a4c207")))
   '(magit-diff-file-heading ((t :inherit bold)))
   '(magit-hash ((((class color) (background light)) :foreground "gray60" :weight light)))
   '(magit-log-author ((((class color) (background light)) :foreground "firebrick" :weight light)))
   '(magit-log-date ((((class color) (background light)) :foreground "gray30" :weight light)))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

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

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc     ((((background light)) :background "#fcf7f2" :foreground "tan1")
                     (((background dark)) :background "#181613" :foreground "bisque4")))
   `(sh-quoted-exec ((((background light)) :background "#faecfa" :foreground "magenta")
                     (((background dark)) :background "#281411" :foreground "salmon")))))

(use-package whitespace
  :defer t
  :config
  (custom-set-faces
   '(whitespace-big-indent ((((background dark)) :foreground "firebrick2")))
   '(whitespace-hspace ((((background dark)) :foreground "firebrick4")))
   '(whitespace-indentation ((((background dark)) :foreground "firebrick4")))
   '(whitespace-newline ((((background dark)) :foreground "firebrick4")))
   '(whitespace-space ((((background dark)) :foreground "firebrick4")))
   '(whitespace-tab ((((background dark)) :foreground "firebrick4")))
   '(whitespace-trailing ((((background dark)) :inverse-video t :foreground "firebrick4")))))

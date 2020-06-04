(use-package twilight-anti-bright-theme
  :ensure t
  :config
  (setq frame-background-mode 'dark)
  (mapc #'frame-set-background-mode (frame-list))
  (load-theme 'twilight-anti-bright t)
  (custom-set-faces
   '(bold ((t :weight bold)))
   '(default ((((background dark)) :foreground "#D1D2D2")))
   '(italic ((t :slant italic)))
   '(cursor ((t :background "sky blue")))
   `(fixed-pitch ((t :font ,alternative-font)))
   `(fixed-pitch-serif ((t :font ,alternative-font)))
   '(link ((t :underline t)))
   '(shadow ((((background light)) :inherit default :foreground "gray55")
             (((background dark))  :inherit default :foreground "gray65")))
   '(show-paren-match ((t :inherit bold :underline t)))
   '(trailing-whitespace ((t :background "gray65")))))

(use-package font-lock
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t :background unspecified :slant unspecified :weight light)))
   '(font-lock-function-name-face ((t :inherit bold)))
   '(font-lock-regexp-grouping-backslash ((((background light)) :inherit font-lock-string-face :background "#E1F2D6")))
   '(font-lock-regexp-grouping-construct ((((background light)) :inherit font-lock-string-face :background "#E1F2D6")))
   '(font-lock-type-face ((((background dark)) :foreground "#B63F1E") (t :weight unspecified)))
   '(font-lock-variable-name-face ((((background light)) :foreground "#607596")))
   '(font-lock-warning-face ((t :inherit (bold italic)))))

  (defvar local-variable-name-light-fg-color
    (saturate-color (color-from 'font-lock-variable-name-face :foreground (if (eq frame-background-mode 'light) 7 -7)) -20)
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
   `(focus-unfocused ((((background light)) :foreground "#A49DA5")))))

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
   '(hl-line ((((background light)) :background "#EEF7FD")))))

(use-package hl-todo
  :defer t
  :config
  (custom-set-faces
   '(hl-todo ((t :inherit bold :weight semi-bold :foreground "#CC9393")))))

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
   `(lsp-diagnostic-level-1 ((t :inherit compilation-error   :font ,alternative-font :weight light)))
   `(lsp-diagnostic-level-2 ((t :inherit compilation-warning :font ,alternative-font :weight light)))
   `(lsp-diagnostic-level-3 ((t :inherit compilation-info    :font ,alternative-font :weight light)))
   `(lsp-diagnostic-level-4 ((t :inherit compilation-info    :font ,alternative-font :weight light)))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((((class color) (background light)) :background "#FBFEEE" :foreground "#A4C207")))
   '(magit-diff-file-heading ((t :inherit bold)))
   '(magit-hash ((((class color) (background light)) :foreground "gray60" :weight light)))
   '(magit-log-author ((((class color) (background light)) :foreground "firebrick" :weight light)))
   '(magit-log-date ((((class color) (background light)) :foreground "gray30" :weight light)))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

(use-package sh-script
  :defer t
  :config
  (custom-set-faces
   `(sh-heredoc     ((((background light)) :background "#FCF7F2" :foreground "tan1")
                     (((background dark)) :background "#181613" :foreground "bisque4")))
   `(sh-quoted-exec ((((background light)) :background "#FAECFA" :foreground "magenta")
                     (((background dark)) :background "#281411" :foreground "salmon")))))

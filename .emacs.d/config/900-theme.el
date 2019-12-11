(use-package twilight-bright-theme
  :ensure t
  :config
  (custom-set-faces
   '(cursor ((t :background "sky blue")))
   '(font-lock-comment-face ((t :background unspecified :weight normal)))
   '(font-lock-doc-face ((t :slant italic)))
   '(font-lock-function-name-face ((t :weight bold)))
   '(font-lock-type-face ((t :weight unspecified)))
   '(font-lock-warning-face ((t :weight extra-bold)))
   '(fringe ((t :background "gray98")))
   '(hl-line ((t :background "#eef7fd")))
   '(link ((t :underline t)))
   '(isearch ((t :background "magenta3" :foreground "white")))
   '(shadow ((t :inherit default :foreground "gray55")))
   '(show-paren-match ((t :underline t)))
   '(trailing-whitespace ((t :background "gray65"))))

  (with-eval-after-load "sh-script"
    (custom-set-faces
     `(sh-heredoc     ((t :background "#fcf7f2" :foreground "tan1")))
     `(sh-quoted-exec ((t :background "#faecfa" :foreground "magenta"))))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face
     ((t :foreground ,(color-from 'default :foreground +4)
         :background ,(color-from 'default :background -2))))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   '(cider-fringe-good-face ((t :inherit success)))
   '(clojure-define-type-face ((t :inherit (font-lock-type-face))))
   '(clojure-defining-spec-face ((t :inherit (clojure-keyword-face))))
   `(clojure-fn-parameter-face ((((class color) (background light)) :foreground ,local-variable-name-light-fg-color :weight ,(face-attribute 'default :weight))))
   '(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   '(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   '(clojure-side-effect-face ((t :inherit (bold italic font-lock-warning-face))))
   '(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))))

(use-package flymake
  :defer t
  :config
  (custom-set-faces
   '(flymake-warning ((t :underline (:style wave :color "dark orange"))))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch)))))

(use-package hl-todo
  :defer t
  :config
  (custom-set-faces
   '(hl-todo ((t :foreground "#cc9393" :weight semi-bold)))))

(use-package iedit
  :defer t
  :config
  (custom-set-faces
   '(iedit-occurrence ((t :inherit (highlight italic))))))

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
            (((class color) (background light) (min-colors 256)) :inherit (shadow default) :background "color-255")))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((((class color) (background light)) :background "#fbfeee" :foreground "#a4c207")))
   '(magit-hash ((((class color) (background light)) :foreground "gray60" :slant normal :weight normal)))
   '(magit-log-author ((((class color) (background light)) :foreground "firebrick" :slant normal :weight normal)))
   '(magit-log-date ((((class color) (background light)) :foreground "gray30" :slant normal :weight normal)))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

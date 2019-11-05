(defvar local-variable-name-light-fg-color
  (saturate-color (color-from 'font-lock-variable-name-face :foreground 7) -20)
  "A foreground color for the local variable such as parameters")

(custom-set-faces
 '(diff-added          ((((class color) (background light)) :background "#cceecc" :foreground "#22aa22")))
 '(diff-changed        ((((class color) (background light)) :background "#eeeecc" :foreground "#aaaa22")))
 '(diff-removed        ((((class color) (background light)) :background "#eecccc" :foreground "#aa2222")))
 '(diff-refine-added   ((((class color) (background light)) :background "#ddffdd" :foreground "#119911")))
 '(diff-refine-changed ((((class color) (background light)) :background "#ffffdd" :foreground "#999911")))
 '(diff-refine-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#991111")))
 '(region ((((class color) (background light)) :background "light sky blue"))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   '(auto-dim-other-buffers-face ((((class color) (background light)) :background "#F3F3F3")))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   `(cider-fringe-good-face ((t :inherit success)))
   `(clojure-define-type-face ((t :inherit (font-lock-type-face))))
   `(clojure-defining-spec-face ((t :inherit (clojure-keyword-face))))
   `(clojure-fn-parameter-face ((((class color) (background light)) :foreground ,local-variable-name-light-fg-color :weight medium)))
   `(clojure-keyword-face ((t :inherit font-lock-builtin-face)))
   `(clojure-local-binding-variable-name-face ((t :inherit clojure-fn-parameter-face)))
   `(clojure-side-effect-face ((t :inherit (bold italic font-lock-warning-face))))
   `(clojure-special-variable-name-face ((t :inherit font-lock-constant-face)))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch)))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:inherit font-lock-variable-name-face))))
   `(php-variable-name ((((class color) (background light))
                         (:foreground ,(-> 'font-lock-variable-name-face
                                           (color-from :foreground 30)
                                           (saturate-color 10)
                                           (mix-color (color-from 'font-lock-string-face :foreground -30)))))))))

(use-package powerline
  :defer t
  :config
  (custom-set-faces
   '(mode-line-inactive ((((class color) (background light))
                          :background "grey90"
                          :foreground "grey40"
                          :weight light
                          :box (:line-width -1 :color "grey75" :style nil))))))

(use-package lsp-mode
  :defer t
  :config
  (custom-set-faces
   `(lsp-face-highlight-read  ((t (:inherit highlight :underline unspecified))))
   `(lsp-face-highlight-write ((t (:inherit highlight :underline t))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((((class color) (background light)) :background "#FBFEEE" :foreground "#A4C207")))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

(use-package leuven-theme
  :ensure t
  :config
  (custom-set-faces
   `(bold ((t (:weight bold))))
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (bold font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (bold clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:inherit font-lock-variable-name-face :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(lazy-highlight ((t (:weight bold))))
   `(lisp-local-binding-variable-name-face ((t (:inherit font-lock-variable-name-face :weight medium)))))
  (custom-theme-set-faces
   'leuven
   `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
   `(helm-match ((t (:inherit lazy-highlight))))
   `(helm-selection ((t (:inherit isearch))))
   `(hl-line ((t (:weight bold))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-warning-face :weight medium))))))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t (:inherit default))))))

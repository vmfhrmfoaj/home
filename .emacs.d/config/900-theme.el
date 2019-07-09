(use-package dracula-theme
  :ensure t
  :init
  (defvar local-variable-name-fg-color
    (-> 'font-lock-variable-name-face
        (color-from :foreground 7)
        (saturate-color -20))
    "TODO")

  (setq x-underline-at-descent-line t))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   `(auto-dim-other-buffers-face ((t (:background ,(color-from 'default :background -3)))))))

(use-package creole-mode
  :defer t
  :config
  (custom-theme-set-faces
   'solarized-light
   `(info-title-1 ((t (:inherit outline-1 :height 1.3))))
   `(info-title-2 ((t (:inherit outline-2 :height 1.2))))
   `(info-title-3 ((t (:inherit outline-3 :height 1.1))))
   `(info-title-4 ((t (:inherit outline-4 :height 1.0))))))

(use-package clojure-mode
  :defer t
  :config
  (custom-set-faces
   `(cider-fringe-good-face ((t (:inherit success))))
   `(clojure-define-type-face   ((t (:inherit (font-lock-type-face)))))
   `(clojure-defining-spec-face ((t (:inherit (clojure-keyword-face)))))
   `(clojure-fn-parameter-face  ((t (:foreground ,local-variable-name-fg-color :weight medium))))
   `(clojure-keyword-face       ((t (:inherit font-lock-builtin-face))))
   `(clojure-local-binding-variable-name-face ((t (:inherit clojure-fn-parameter-face))))
   `(clojure-side-effect-face   ((t (:inherit (bold italic font-lock-warning-face)))))
   `(clojure-special-variable-name-face ((t (:inherit font-lock-variable-name-face))))))

(use-package elisp-mode
  :defer t
  :config
  (custom-set-faces
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color :weight medium))))))

(use-package go-mode
  :defer t
  :config
  (custom-set-faces
   `(go-argument-name-face ((t (:inherit font-lock-variable-name-face))))))

(use-package highlight-symbol
  :defer t
  :config
  (custom-set-faces
   `(highlight-symbol-face ((t (:background ,(color-from 'default :background -3)))))))

(use-package linum
  :defer t
  :config
  (custom-set-faces
   `(linum ((t (:inherit default))))))

(use-package linum-relative
  :defer t
  :config
  (custom-set-faces
   `(linum-relative-current-face ((t (:inherit linum :background unspecified :foreground "#CAE682"))))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:inherit font-lock-variable-name-face))))
   `(php-variable-name ((t (:foreground ,(-> 'font-lock-variable-name-face
                                             (color-from :foreground 30)
                                             (saturate-color 10)
                                             (mix-color (color-from 'font-lock-string-face :foreground -30)))))))))

(use-package rpm-spec-mode
  :defer t
  :config
  (set-face-attribute 'rpm-spec-section-face nil
                      :underline 'unspecified
                      :inherit 'underline))

(use-package web-mode
  :disabled t
  :defer t
  :config
  (let ((tag-face       `(:foreground ,(-> "#AE1B9A" (light-color 15) (saturate-color -40))))
        (attr-name-face `(:foreground ,(-> "#F36335" (light-color 15) (saturate-color -40))))
        (attr-var-face  `(:foreground ,(-> "green4"  (light-color 15) (saturate-color -40)))))
   (custom-set-faces
    `(web-mode-html-tag-bracket-face ((t ,tag-face)))
    `(web-mode-html-tag-face ((t ,tag-face)))
    `(web-mode-html-attr-name-face ((t ,attr-name-face)))
    `(web-mode-html-attr-value-face ((t ,attr-var-face))))))

(defface clojure-defining-ns-face
  `((t (:inherit font-lock-type-face :weight bold)))
  "Face used to font-lock Clojure defining namespace.")

(defface clojure-defining-spec-face
  `((t (:inherit clojure-keyword-face :weight bold)))
  "Face used to font-lock Clojure defining Spec")

(defface clojure-side-effect-face
  `((t (:inherit font-lock-warning-face :slant italic :weight bold)))
  "Face used to font-lock Clojure side-effect indicator.")

(defface clojure-important-keywords-face
  '((t (:inherit font-lock-keyword-face :slant italic)))
  "Face used to font-lock Clojure important keywords.")

(defface clojure-special-variable-name-face
  '((t (:inherit font-lock-variable-name-face :weight normal)))
  "Face used to font-lock Clojure special variable name.")

(defface clojure-local-binding-variable-name-face
  '((t (:inherit font-lock-variable-name-face :weight normal)))
  "Face used to font-lock Clojure local binding variable name.")

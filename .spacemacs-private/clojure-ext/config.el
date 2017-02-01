(defface clojure-define-namespace
  `((t :inherit font-lock-type-face
       :weight bold))
  "Face used to font-lock Clojure defining namespace.")

(defface clojure-side-effect-face
  `((t :inherit font-lock-warning-face
       :slant italic
       :weight bold))
  "Face used to font-lock Clojure side-effect indicator.")

(defface clojure-important-keywords-face
  '((t :inherit font-lock-keyword-face :slant italic))
  "Face used to font-lock Clojure important keywords.")

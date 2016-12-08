(defface clojure-side-effect-face
  `((t :foreground ,(face-attribute 'font-lock-warning-face :foreground)
       :slant italic
       :weight bold))
  "Face used to font-lock Clojure side-effect indicator.")

(defface clojure-define-type-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face used to font-lock Clojure define type.")

(defface clojure-just-variable-name-face
  '((t :inherit font-lock-variable-name-face
       :weight normal))
  "Face used to font-lock Clojure variable name.")

(defface clojure-important-keywords-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used to font-lock Clojure important keywords.")

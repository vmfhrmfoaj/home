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

(defface clojure-fn-parameter-face
  '((t (:inherit font-lock-variable-name-face :weight normal)))
  "Face used to font-lock Clojure parameter.")

(defface clojure-semi-function-name-face
  '((t (:inherit font-lock-function-name-face :weight normal)))
  "Face used to font-lock Clojure OOP style functions.")

(defface clojure-cond-condtion-face
  '((t (:slant italic)))
  "Face used to font-lock Clojure conditions in `cond' form.")

(defface clojure-if-true-face
  '((t (:inherit default)))
  "Face used to font-lock Clojure `if' true form.")


(defcustom clojure--ignore-binding-highlight-keywords
  '("_" "&")
  "TODO"
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom clojure--binding-forms
  '("binding" "doseq" "dotimes" "for" "let" "if-let" "if-some" "when-let" "when-some" "loop" "with-redefs")
  "List of Clojure binding form."
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom clojure-spc-key-valign-skip 3
  "TODO"
  :type 'integer
  :safe 'integerp)

(defun fake-match-2 ()
  "TODO"
  (-repeat 2 (point-min-marker)))

(defun fake-match-4 ()
  "TODO"
  (-repeat 4 (point-min-marker)))

(use-package cperl-mode
  :defer t
  :config
  (font-lock-add-keywords
   'cperl-mode
   (let* ((symbol "[@$%]+[:_0-9a-zA-Z]+")
          (whitespace "[ \r\t\n]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*")))
     `((,(concat "\\(" symbol "\\|\\(accept\\|do\\)\\s-*(\\)")
        (1 (cond
            ((sp-point-in-string)  'font-lock-string-face)
            ((sp-point-in-comment) 'font-lock-comment-face)
            (t nil))
           t))
       (,(concat "^" whitespace* "\\(sub\\)" whitespace+ "\\([_0-9A-Za-z]+\\)\\(?:" whitespace* "([@$%]*)\\)?")
        (1 'font-lock-keyword-face)
        (2 'font-lock-function-name-face t))
       (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "\\(" symbol "\\)")
        (1 'font-lock-variable-name-face))
       (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "(" )
        (,(concat "\\(" symbol "\\)")
         (save-excursion
           (safe-up-list-1)
           (point))
         nil
         (1 'font-lock-variable-name-face)))
       ("for\\(each\\)? my \\([@$%][_0-9a-zA-Z]+\\)"
        (1 'font-lock-variable-name-face))
       (,(concat whitespace "\\(accept\\)" whitespace* "(")
        (1 'font-lock-type-face))))
   :append))

(use-package clojure-mode
  :defer t
  :init
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
  (defface clojure-local-binding-variable-name-warning-face
    '((t (:inherit clojure-local-binding-variable-name-face :slant italic)))
    "TODO")
  (defface clojure-fn-parameter-face
    '((t (:inherit font-lock-variable-name-face :weight normal)))
    "Face used to font-lock Clojure parameter.")
  (defface clojure-fn-parameter-warning-face
    '((t (:inherit clojure-fn-parameter-face :slant italic)))
    "TODO")
  (defface clojure-semi-function-name-face
    '((t (:inherit font-lock-function-name-face :weight normal)))
    "Face used to font-lock Clojure OOP style functions.")
  (defface clojure-cond-condtion-face
    '((t (:slant italic)))
    "Face used to font-lock Clojure conditions in `cond' form.")
  (defface clojure-if-true-face
    '((t (:inherit default)))
    "Face used to font-lock Clojure `if' true form.")
  (defface clojure-define-type-face
    '((t (:inherit font-lock-type-face :weight bold)))
    "TODO")
  (defface clojure-meta-face
    '((t (:inherit shadow)))
    "TODO")
  (defface clojure-interop-method-face
    '((t (:inherit font-lock-keyword-face)))
    "TODO")

  (defcustom clojure--ignore-binding-highlight-regex
    (concat "^\\("
            "_\\|"
            "&\\)$")
    "TODO"
    :type '(repeat string)
    :safe (lambda (value)
            (and (listp value)
                 (cl-every 'stringp value))))

  (defcustom clojure-spc-key-valign-skip 3
    "TODO"
    :type 'integer
    :safe 'integerp)

  (defvar clojure--binding-forms
    '("binding" "doseq" "dotimes" "for" "let" "if-let" "if-some" "when-let" "when-some" "loop" "with-redefs")
    "List of Clojure binding form.")

  (defun clojure-forward-sexp (&optional n)
    "TODO"
    (or n (setq n 1))
    (while (not (zerop n))
      (if (> n 0) (clojure-skip  1 :comment :ignored-form :tagged-literal))
      (forward-sexp (if (< 0 n) 1 -1))
      (if (< n 0) (clojure-skip -1 :comment :ignored-form :tagged-literal))
      (setq n (funcall (if (< 0 n) '1- '1+) n))))

  :config
  (setq clojure-cond-form--point nil)
  (setq clojure-if-form--point nil)
  (setq clojure-interface-form--point nil)
  (setq clojure-binding-form--point nil)
  (setq clojure-binding-form--point nil)
  (setq clojure-binding-form--recursive-point nil)
  (setq clojure-binding-form--recursive-limit nil)
  (setq clojure-oop-kw--str nil)
  (setq clojure-oop-kw--point nil)
  (setq clojure-oop-fn-form--points nil)
  (setq clojure-oop-fn-form--point nil)
  (setq clojure-oop-fn-form--point nil)
  (setq clojure-oop-fn-recursive--point nil)
  (setq clojure-oop-fn-recursive--limit nil)
  (setq clojure-fn-form--point nil)
  (setq clojure-fn-form--method? nil)
  (setq clojure-fn-form--multi-arity? nil)
  (setq clojure-fn-recursive--point nil)
  (setq clojure-fn-recursive--limit nil)
  (setq clojure-meta---point nil)
  (make-local-variable 'clojure-cond-form--point)
  (make-local-variable 'clojure-if-form--point)
  (make-local-variable 'clojure-interface-form--point)
  (make-local-variable 'clojure-binding-form--point)
  (make-local-variable 'clojure-binding-form--point)
  (make-local-variable 'clojure-binding-form--recursive-point)
  (make-local-variable 'clojure-binding-form--recursive-limit)
  (make-local-variable 'clojure-oop-kw--str)
  (make-local-variable 'clojure-oop-kw--point)
  (make-local-variable 'clojure-oop-fn-form--points)
  (make-local-variable 'clojure-oop-fn-form--point)
  (make-local-variable 'clojure-oop-fn-form--point)
  (make-local-variable 'clojure-oop-fn-recursive--point)
  (make-local-variable 'clojure-oop-fn-recursive--limit)
  (make-local-variable 'clojure-fn-form--point)
  (make-local-variable 'clojure-fn-form--method?)
  (make-local-variable 'clojure-fn-form--multi-arity?)
  (make-local-variable 'clojure-fn-recursive--point)
  (make-local-variable 'clojure-fn-recursive--limit)
  (make-local-variable 'clojure-meta---point)

  (let* ((whitespace "[ \r\t\n]")
         (whitespace+ (concat whitespace "+"))
         (whitespace* (concat whitespace "*"))
         (symbol clojure--sym-regexp)
         (symbol? (concat "\\(?:" symbol "\\)?"))
         (namespace (concat "\\(?:" symbol "/\\)"))
         (namespace? (concat namespace "?"))
         (meta? "\\(?:\\(?:\\^{[^^]+}\\|\\^:?\\sw+\\)[ \r\n\t]+\\)?")
         (core-ns  (concat (regexp-opt '("clojure.core" "cljs.core" "core") nil) "/"))
         (core-ns? (concat "\\(?:" core-ns "\\)?"))
         (if-kw (regexp-opt '("if" "if-some" "if-let" "if-not")))
         (oop-kw (regexp-opt '("definterface" "defprotocol" "defrecord" "deftype" "extend-protocol" "extend-type" "proxy" "reify")))
         (def-kw (regexp-opt '("defmacro" "defn" "defn-" "defmethod" "fn" "defrecord" "deftype") t))
         (no-kw-kw (regexp-opt '("->" "->>" "as->" "as->>" "some->" "some->>" "and" "or") t))
         (clj-kw (regexp-opt '("go-loop" "with-hard-redefs") t))
         (important-kw (regexp-opt '("case" "cond" "condp" "cond->" "cond->>" "for" "if" "if-let" "if-not" "recur" "throw" "when"
                                     "when-let" "when-not" "while") t)))
    ;; TODO
    ;;  refactoring
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      ;; append rules
      (font-lock-add-keywords
       mode
       `(;; Highlight condtions in `cond' form.
         (,(concat "(" core-ns? "\\(cond\\(?:->>?\\)?\\)[ \r\t\n]+")
          (,(lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (when (> limit (point))
                  (clojure-skip :comment :ignored-form)
                  (set-match-data (list (point-marker) (progn (forward-sexp) (point-marker))))
                  (clojure-forward-sexp)
                  t)))
           (prog1 (save-excursion
                    (if (in-comment?)
                        (setq font-lock--skip t)
                      (setq font-lock--skip nil)
                      (setq clojure-cond-form--point (point))
                      (safe-up-list-1)
                      (point)))
             (when (string-match-p "->>?" (match-string 1))
               (condition-case nil
                   (clojure-forward-sexp)
                 (error (setq font-lock--skip t)))))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-cond-form--point))
           (0 'clojure-cond-condtion-face prepend)))
         (,(concat "(" core-ns? if-kw "[ \r\t\n]+")
          (,(lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (when (> limit (point))
                  (clojure-forward-sexp)
                  (set-match-data (list (progn (clojure-skip :comment :ignored-form) (point-marker))
                                        (progn (clojure-forward-sexp) (point-marker))))
                  (clojure-forward-sexp)
                  t)))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-if-form--point (point))
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-if-form--point))
           (0 'clojure-if-true-face append)))
         ;; DSL
         ;; - CSS
         (,(concat "(" namespace? "css[ \r\t\n]")
          ("\\(#[0-9A-Fa-f]\\{3,6\\}\\)"
           (save-excursion
             (up-list)
             (point))
           nil
           (0 (let* ((max 255.0)
                     (str (string-to-list (match-string 0)))
                     (bg_ (face-attribute 'default :background))
                     (bg  (if (= 7 (length str))
                              (apply #'string str)
                            (->> str
                                 (--remove-first (char-equal ?# it))
                                 (-take 3)
                                 (--map (make-string 2 it))
                                 (apply #'concat "#"))))
                     (di (if (> 294784 (color-distance "black" bg_)) (+ 1) (- 1)))
                     (fg (dim-color bg (* di 40))))
                (if (or (> 2500 (color-distance bg bg_))
                        (> 5000 (color-distance bg fg)))
                    (list :underline (> 2500 (color-distance bg bg_))
                          :foreground bg
                          :distant-foreground (light-color bg (* di 30)))
                  `(:inverse-video t :foreground ,bg :background ,fg)))
              t)))
         ;; Improve docstring
         (,(concat "(defprotocol" whitespace+ symbol "\\>")
          ,(-partial
            (lambda (meta?+symbol limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (re-search-forward meta?+symbol limit)
                (clojure-skip :vector)
                (set-match-data
                 (if (looking-at-p "\"")
                     (list (point-marker) (progn (forward-sexp) (point-marker)))
                   (fake-match-2)))
                t))
            (concat "(" meta? symbol "\\>"))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq clojure-interface-form--point (point))
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char clojure-interface-form--point))
          (0 'font-lock-doc-face t))
         (,(-partial
            (lambda (symbol+alter-mark limit)
              (when (re-search-forward symbol+alter-mark limit 'no-err)
                (let ((face (plist-get (text-properties-at (match-beginning 1)) 'face))
                      (ignore-faces '(font-lock-doc-face
                                      font-lock-string-face
                                      font-lock-comment-face)))
                  (when (memq face ignore-faces)
                    (set-match-data (fake-match-4)))
                  t)))
            (concat symbol "?\\(!+\\)\\>"))
          (1 'clojure-side-effect-face t))
         (,(concat "\\(" symbol "\\(\\." symbol "\\)+\\)")
          (1 'font-lock-type-face nil))
         (,(concat "\\<\\(\\([A-Z]+[0-9a-z]+\\)+\\)\\>")
          (1 'font-lock-type-face nil)))
       :append)
      ;; prepend rules
      (font-lock-add-keywords
       mode
       `(;; Meta
         (,(concat "\\(?:" whitespace "\\|[([{]\\)\\^[:A-Za-z{]")
          (,(lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (let ((start (progn
                               (backward-char 1)
                               (point-marker))))
                  (goto-char limit)
                  (set-match-data (list start (point-marker)))
                  t)))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-meta---point (point))
               (backward-char 1)
               (clojure-forward-sexp)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-meta---point))
           (0 'clojure-meta-face t)))
         ;; Binding forms
         (,(concat "(" core-ns? (regexp-opt clojure--binding-forms) "[ \r\t\n]+\\[")
          ;; Normal bindings
          (,(-partial
             (lambda (meta?+ns?+symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (clojure-skip :comment :ignored-form :type-hint :destructuring-bind)
                 (let ((local-limit (save-excursion (forward-sexp) (point))))
                   (unless (and (re-search-forward meta?+ns?+symbol (min local-limit limit) t)
                                (not (string-match-p clojure--ignore-binding-highlight-regex
                                                     (match-string-no-properties 1))))
                     (set-match-data (fake-match-4)))
                   (goto-char local-limit))
                 (clojure-forward-sexp)
                 t))
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)"))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-binding-form--point (point))
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-binding-form--point))
           (1 'clojure-local-binding-variable-name-face))
          ;; Destructuring bindings
          (,(-partial
             (lambda (meta?+ns?+symbol limit)
               ;; NOTE
               ;; We need to iterate to search symbols in the destructuring form,
               ;; but anchored-matcher does not support recursion.
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (unless clojure-binding-form--recursive-point
                   (while (and (> limit (point))
                               (prog1 t (clojure-skip :comment :ignored-form :type-hint))
                               ;; skip normal bind?
                               (not (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)"))
                               (prog1 t (clojure-forward-sexp 2))))
                   (when (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)")
                     (setq clojure-binding-form--recursive-point (progn (down-list) (point))
                           clojure-binding-form--recursive-limit (save-excursion (up-list) (point)))))
                 (when clojure-binding-form--recursive-point
                   (clojure-skip :comment :ignored-form :type-hint)
                   (if (re-search-forward meta?+ns?+symbol
                                          (min limit clojure-binding-form--recursive-limit) t)
                       (progn
                         ;; ignores
                         (when (string-match-p clojure--ignore-binding-highlight-regex
                                               (match-string-no-properties 1))
                           (set-match-data (fake-match-4)))
                         ;; Handle default bind map
                         (when (save-excursion
                                 (backward-up-list)
                                 (and (char-equal ?{ (char-after))
                                      (ignore-errors
                                        (clojure-forward-sexp -1)
                                        (looking-at-p ":or\\>"))))
                           (clojure-forward-sexp)))
                     (goto-char clojure-binding-form--recursive-limit)
                     (clojure-forward-sexp)
                     (setq clojure-binding-form--recursive-point nil
                           clojure-binding-form--recursive-limit nil)
                     (set-match-data (fake-match-4)))
                   t)))
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>"))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-binding-form--point (point))
               (setq clojure-binding-form--recursive-point nil)
               (setq clojure-binding-form--recursive-limit nil)
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-binding-form--point))
           (1 'clojure-local-binding-variable-name-face)))
         ;; OOP style function forms & letfn
         (,(concat "(" core-ns? "\\(" oop-kw whitespace+ meta? "\\|" "letfn" whitespace+ "\\[" "\\)")
          ;; highlighting definition type symbol
          (,(-partial
             (lambda (symbol limit)
               (unless font-lock--skip
                 (and (string-match-p "^def" clojure-oop-kw--str)
                      (re-search-forward symbol limit t))))
             (concat "\\(" symbol "\\)"))
           (save-excursion
             (setq clojure-oop-kw--str (match-string-no-properties 1))
             (setq clojure-oop-kw--point (point))
             (condition-case nil
                 (clojure-forward-sexp)
               (error (setq font-lock--skip t)))
             (point))
           (goto-char clojure-oop-kw--point)
           (0 'clojure-define-type-face))
          ;; highlighting OOP fn name
          (,(-partial
             (lambda (symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (while (and (> limit (point))
                             (prog1 t (clojure-skip :comment :ignored-form))
                             (not (looking-at-p "[ \r\t\n]*("))
                             (prog1 t (forward-sexp))))
                 (when (looking-at-p "[ \r\t\n]*(")
                   (down-list)
                   (clojure-skip :type-hint :ignored-form)
                   (let ((local-limit (save-excursion (forward-sexp) (point))))
                     (if (re-search-forward symbol (min limit local-limit) t)
                         (add-to-list 'clojure-oop-fn-form--points (match-end 0))
                       (set-match-data (fake-match-2))))
                   (up-list)
                   t)))
             (concat symbol "\\>"))
           (save-excursion
             (setq clojure-oop-fn-form--points nil)
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-oop-fn-form--point (point))
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-oop-fn-form--point))
           (0 'clojure-semi-function-name-face))
          ;; highlighting OOP fn parameters
          (,(-partial
             (lambda (meta?+ns?+symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (while (and (not clojure-oop-fn-recursive--point)
                             clojure-oop-fn-form--points)
                   (-when-let (point (car clojure-oop-fn-form--points))
                     (setq clojure-oop-fn-form--points (cdr clojure-oop-fn-form--points))
                     (goto-char point)
                     (when (re-search-forward "\\[" limit 'noerr)
                       (setq clojure-oop-fn-recursive--point (point)
                             clojure-oop-fn-recursive--limit (save-excursion
                                                               (up-list)
                                                               (point)))
                       (when (string-match-p "definterface\\|defprotocol" clojure-oop-kw--str)
                         (setq clojure-oop-fn-form--points
                               (cons clojure-oop-fn-recursive--limit
                                     clojure-oop-fn-form--points))))))
                 (when clojure-oop-fn-recursive--point
                   (if (re-search-forward meta?+ns?+symbol
                                          (min limit clojure-oop-fn-recursive--limit) t)
                       (when (string-match-p clojure--ignore-binding-highlight-regex
                                             (match-string-no-properties 1))
                         (set-match-data (fake-match-4)))
                     (set-match-data (fake-match-4))
                     (setq clojure-oop-fn-recursive--point nil
                           clojure-oop-fn-recursive--limit nil))
                   t)))
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>"))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-oop-fn-form--point (point))
               (setq clojure-oop-fn-recursive--point nil)
               (setq clojure-oop-fn-recursive--limit nil)
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-oop-fn-form--point))
           (1 'clojure-fn-parameter-face)))
         ;; Removes(overwrite) rules
         (,(concat "(" namespace?
                   "\\(default/?[^" clojure--sym-forbidden-rest-chars "]*\\)"
                   whitespace*
                   "\\(" symbol "\\)\\>")
          (1 'default)
          (2 'default))
         (,(concat "(" namespace? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                   whitespace+
                   meta?
                   "::?" namespace? "\\(" symbol "\\)\\>")
          (1 'font-lock-keyword-face)
          (2 'clojure-defining-spec-face))
         (,(concat "(" core-ns? def-kw "\\>" whitespace+ meta? "\\(" symbol? "\\)")
          (1 'font-lock-keyword-face)
          ;; NOTE
          ;; Clojure is a Lisp-1.
          ;; It is mean the symbol of the variable and the function are no different.
          (2 (cond
              ((string-match-p "defrecord\\|deftype" (match-string 1))
               'clojure-define-type-face)
              ((string-match-p "defmacro\\|^fn" (match-string 1))
               'font-lock-function-name-face)
              (t 'font-lock-variable-name-face)))
          ;; fn parameters highlight
          (,(-partial
             (lambda (meta?+ns?+symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (unless clojure-fn-recursive--point
                   (when clojure-fn-form--multi-arity?
                     (up-list 2))
                   (while (progn
                            (clojure-skip :comment :ignored-form :string :map)
                            (when clojure-fn-form--method?
                              (setq clojure-fn-form--method? nil)
                              (clojure-forward-sexp)
                              t)))
                   (when (looking-at "(")
                     (setq clojure-fn-form--multi-arity? t)
                     (down-list))
                   (when (looking-at "\\[")
                     (down-list)
                     (setq clojure-fn-recursive--point (point)
                           clojure-fn-recursive--limit (save-excursion
                                                         (up-list)
                                                         (1- (point))))))
                 (when clojure-fn-recursive--point
                   (if (re-search-forward meta?+ns?+symbol
                                          (min limit clojure-fn-recursive--limit) t)
                       (progn
                         ;; ignores
                         (when (string-match-p clojure--ignore-binding-highlight-regex
                                               (match-string-no-properties 1))
                           (set-match-data (fake-match-4)))
                         ;; Handle default bind map
                         (when (save-excursion
                                 (backward-up-list)
                                 (and (char-equal ?{ (char-after))
                                      (ignore-errors
                                        (clojure-forward-sexp -1)
                                        (looking-at-p ":or\\>"))))
                           (clojure-forward-sexp)))
                     (set-match-data (fake-match-4))
                     (setq clojure-fn-recursive--point nil
                           clojure-fn-recursive--limit nil))
                   t)))
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>"))
           (if (in-comment?)
               (setq font-lock--skip t)
             (setq font-lock--skip nil)
             (setq clojure-fn-form--point (point))
             (setq clojure-fn-form--method? (string-match-p "defmethod" (match-string 1)))
             (setq clojure-fn-form--multi-arity? nil)
             (setq clojure-fn-recursive--point nil)
             (setq clojure-fn-recursive--limit nil)
             (save-excursion
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-fn-form--point))
           (1 'clojure-fn-parameter-face)))
         (,(concat "(" core-ns? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                   whitespace+
                   meta?
                   namespace? "\\(" symbol "\\)")
          (1 'font-lock-keyword-face)
          (2 'font-lock-variable-name-face))
         ("\\<%[&1-9]?\\>"
          (0 'clojure-special-variable-name-face))
         (,(concat "\\(?:\\<\\|/\\)@?"
                   "\\(\\*[^" clojure--sym-forbidden-rest-chars "*]*\\*\\)\\>")
          (1 'clojure-special-variable-name-face))
         (,(concat "(" no-kw-kw)
          (1 'default))
         ;; Adds rules
         (,(concat "\\^\\(" symbol "\\)\\>")
          (1 'font-lock-type-face))
         (,(concat "\\(#js\\)"
                   whitespace*
                   "\\s(")
          (1 'font-lock-builtin-face))
         (,(concat "(" namespace? clj-kw)
          (1 'font-lock-keyword-face))
         (,(concat "(\\(\\.\\.\\) " symbol)
          (1 'clojure-interop-method-face)
          (,(concat "\\<\\(-\\)" symbol)
           (save-excursion
             (up-list)
             (point))
           nil
           (1 'clojure-interop-method-face)))
         (,(concat "(\\(\\.-?\\)" symbol)
          (1 'clojure-interop-method-face))
         (,(concat "(" symbol "\\(\\.\\)" whitespace)
          (1 'clojure-interop-method-face))
         (,(concat "(" important-kw "\\(?:)\\|" whitespace "\\)")
          (1 'clojure-important-keywords-face))
         (,(concat "::\\(" symbol "\\)/" symbol "\\>")
          (1 'font-lock-type-face)))))))

(use-package cperl-mode
  :defer t
  :config
  (font-lock-add-keywords
   'cperl-mode
   (let* ((symbol "[@$%]+[:_0-9a-zA-Z]+")
          (whitespace "[ \r\t\n]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*")))
     `((,(concat "\\(" symbol "\\|\\(accept\\|do\\)\\s-*(\\)")
        (1 (cond
            ((sp-point-in-string)  'font-lock-string-face)
            ((sp-point-in-comment) 'font-lock-comment-face)
            (t nil))
           t))
       (,(concat "^" whitespace* "\\(sub\\)" whitespace+ "\\([_0-9A-Za-z]+\\)\\(?:" whitespace* "([@$%]*)\\)?")
        (1 'font-lock-keyword-face)
        (2 'font-lock-function-name-face t))
       (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "\\(" symbol "\\)")
        (1 'font-lock-variable-name-face))
       (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "(" )
        (,(concat "\\(" symbol "\\)")
         (save-excursion
           (safe-up-list-1)
           (point))
         nil
         (1 'font-lock-variable-name-face)))
       ("for\\(each\\)? my \\([@$%][_0-9a-zA-Z]+\\)"
        (1 'font-lock-variable-name-face))
       (,(concat whitespace "\\(accept\\)" whitespace* "(")
        (1 'font-lock-type-face))))
   :append))

(use-package elisp-mode
  :defer t
  :init
  (defface lisp-local-binding-variable-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Lisp local binding variable name.")

  :config
  (setq-default elisp--binding-form-point nil)
  (make-local-variable 'elisp--binding-form-point)
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (font-lock-add-keywords
     mode
     (let* ((symbol "[-+*/=>$&?:_0-9a-zA-Z]+")
            (whitespace "[ \r\t\n]")
            (whitespace+ (concat whitespace "+"))
            (whitespace* (concat whitespace "*")))
       `(("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
          1 'default nil)
         ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
          1 'font-lock-constant-face)
         ("(\\(assert\\)"
          1 'font-lock-warning-face)
         ("\\s-\\(\\?[A-Za-z]\\)\\>"
          1 'font-lock-string-face)
         ;; local variables
         (,(concat "(\\(lexical-\\)?let\\*?" whitespace+ "(")
          (,(-partial
             (lambda (symbol+whitespace limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (comment-forward (point-max))
                 (let ((local-limit (save-excursion (forward-sexp) (point))))
                   (unless (and (re-search-forward symbol+whitespace (min local-limit limit) t)
                                (ignore-errors (forward-sexp) t))
                     (set-match-data (fake-match-4)))
                   (goto-char local-limit))
                 t))
             (concat "(\\(" symbol "\\)" whitespace+))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq elisp--binding-form-point (point))
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char elisp--binding-form-point))
           (1 'lisp-local-binding-variable-name-face)))
         ;; function arguments
         (,(concat "\\(defun\\|lambda\\)" whitespace+ "\\(" symbol whitespace+ "\\)?(")
          (,(-partial
             (lambda (symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (when (re-search-forward symbol limit t)
                   (when (string-match-p "^&" (match-string 1))
                     (set-match-data (fake-match-4)))
                   t)))
             (concat "\\(" symbol "\\)\\>"))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq elisp--binding-form-point (point))
               (safe-up-list-1)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char elisp--binding-form-point))
           (1 'lisp-local-binding-variable-name-face)))
         (,(concat "(-\\(?:when\\|if\\)-let\\*?" whitespace+ "(\\(" symbol "\\)" whitespace)
          (1 'lisp-local-binding-variable-name-face)))))))

(use-package org
  :defer t
  :config
  (when (require 'all-the-icons nil t)
    (font-lock-add-keywords
     'org-mode
     (let* ((data (all-the-icons-faicon-data))
            (square       (string-to-char (cdr (assoc "square" data))))
            (minus-square (string-to-char (cdr (assoc "minus-square" data))))
            (check-square (string-to-char (cdr (assoc "check-square" data)))))
       `(("^\\s-*\\(-\\) "
          1 (progn
              (let ((s (match-beginning 1))
                    (e (match-end 1)))
                (compose-region s e ?╺))
              'bold))
         ("^\\s-*\\(\\([0-9]\\.\\)\\) "
          1 'bold)
         ("^\\s-*\\(?:-\\|[0-9]+\\.\\) \\(\\[\\( \\|-\\|X\\)\\]\\) "
          1 (progn
              (let ((x (match-string 2))
                    (s (match-beginning 1))
                    (e (match-end 1)))
                (compose-region
                 s e
                 (cond
                  ((string-equal x " ") ,square)
                  ((string-equal x "-") ,minus-square)
                  ((string-equal x "X") ,check-square)))
                (when (eq 'gnu/linux system-type)
                  (put-text-property s e 'display '(raise 0.1)))
                (list :family "FontAwesome"
                      :foreground (face-attribute (if (string-equal x "X")
                                                      'org-done 'org-todo)
                                                  :foreground)
                      :height (if (eq 'gnu/linux system-type) 0.95 1))))
          t)
         ("\\(\\\\\\\\\\)\\s-*$"
          1 'shadow nil)))
     :append)))

(use-package php-mode
  :defer t
  :config
  (font-lock-add-keywords
   'php-mode
   (let* ((symbol "\\$[_0-9a-zA-Z]+")
          (whitespace "[ \r\t\n]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*"))
          (assigment (concat whitespace* "[.+-*/]?=[^=>]")))
     `((,(concat "\\(" symbol "\\)")
        (1 (cond
            ((sp-point-in-string)  'font-lock-string-face)
            ((sp-point-in-comment) 'font-lock-comment-face)
            (t nil))
           t))
       ("->\\([_0-9a-zA-Z]+\\)"
        (1 (cond
            ((sp-point-in-string)  'font-lock-string-face)
            ((sp-point-in-comment) 'font-lock-comment-face)
            (t nil))
           t))
       (,(concat "\\(" symbol "\\)\\(\\[[^]]*\\]\\)*" assigment)
        (1 'font-lock-variable-name-face))
       (,(concat symbol "->\\([_0-9a-zA-Z]+\\)\\(\\[[^]]*\\]\\)*" assigment)
        (1 'font-lock-variable-name-face))
       (,(concat "list(\\(" "\\(\"[_0-9A-Za-z]+\"" whitespace* "=>" whitespace* "\\)?" symbol whitespace* ",?" whitespace* "\\)+)" assigment)
        (,(concat "\\(" symbol "\\)")
         (progn
           (goto-char (match-beginning 0))
           (safe-down-list-1)
           (save-excursion
             (safe-up-list-1)
             (point)))
         nil
         (1 'font-lock-variable-name-face)))))
   :append))

(use-package prog-mode
  :defer t
  :init
  ;; - https://gist.github.com/mordocai/50783defab3c3d1650e068b4d1c91495
  (defconst fira-code-font-lock-keywords-alist
    (mapcar (lambda (regex-char-pair)
              (let ((face (if (eq 'gnu/linux system-type)
                              '((:slant normal :weight normal))
                            '((:slant normal)))))
                `(,(car regex-char-pair)
                  (1 (progn
                       (compose-region (match-beginning 1)
                                       (match-end 1)
                                       ;; The first argument to concat is a string containing a literal tab
                                       ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))
                       ',face)
                     prepend))))
            '(;;                             #Xe
              ;;www                          #Xe
              ("\\(www\\)"                   #Xe100)
              ;; **                          #Xe101
              ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
              ("\\(\\*\\*\\*\\)"             #Xe102)
              ;; **/                         #Xe103
              ("\\(\\*\\*/\\)"               #Xe103)
              ("\\(\\*>\\)"                  #Xe104)
              ;; */                          #Xe103
              ;; ("[^*]\\(\\*/\\)"              #Xe105)
              ;; \\                          #Xe
              ("\\(\\\\\\\\\\)"              #Xe106)
              ;; \\\                         #Xe
              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
              ("\\({-\\)"                    #Xe108)
              ("\\(::\\)"                    #Xe10a)
              ("\\(:::\\)"                   #Xe10b)
              ;; ("[^=]\\(:=\\)"                #Xe10c)
              ;; ("\\(!!\\)"                    #Xe10d)
              ("\\(!=\\)"                    #Xe10e)
              ("\\(!==\\)"                   #Xe10f)
              ("\\(-}\\)"                    #Xe110)
              ;; ("\\(--\\)"                    #Xe111)
              ;; ("\\(---\\)"                   #Xe112)
              ("\\(-->\\)"                   #Xe113)
              ("[^-]\\(->\\)"                #Xe114)
              ("\\(->>\\)"                   #Xe115)
              ("\\(-<\\)"                    #Xe116)
              ("\\(-<<\\)"                   #Xe117)
              ("\\(-~\\)"                    #Xe118)
              ;; ("\\(#{\\)"                    #Xe119)
              ;; ("\\(#\\[\\)"                  #Xe11a)
              ;; ("\\(##\\)"                    #Xe11b)
              ;; ("\\(###\\)"                   #Xe11c)
              ;; ("\\(####\\)"                  #Xe11d)
              ;; ("\\(#(\\)"                    #Xe11e)
              ("\\(#\\?\\)"                  #Xe11f)
              ("\\(#_\\)"                    #Xe120)
              ;; ("\\(#_(\\)"                   #Xe121)
              ("\\(\\.-\\)"                  #Xe122)
              ("\\(\\.=\\)"                  #Xe123)
              ;; ..                          #Xe124
              ("\\(\\.\\.\\)"                #Xe124)
              ;; ..<                         #Xe125
              ("\\(\\.\\.<\\)"               #Xe125)
              ;; ...                         #Xe126
              ("\\(\\.\\.\\.\\)"             #Xe126)
              ;; ("\\(\\?=\\)"                  #Xe127)
              ;; ("\\(\\?\\?\\)"                #Xe128)
              ;; ("\\(;;\\)[^;]"                #Xe129)
              ;; /*                          #Xe12a
              ;; ("\\(/\\*\\)"                  #Xe12a)
              ;; /**                         #Xe12b
              ("\\(/\\*\\*\\)"               #Xe12b)
              ("\\(/=\\)"                    #Xe12c)
              ("\\(/==\\)"                   #Xe12d)
              ;; ("\\(/>\\)"                    #Xe12e)
              ("\\(//\\)"                    #Xe12f)
              ("\\(///\\)"                   #Xe130)
              ;; ("\\(&&\\)"                    #Xe131)
              ;; ("\\(||\\)"                    #Xe132)
              ("\\(||=\\)"                   #Xe133)
              ("[^|]\\(|=\\)"                #Xe134)
              ("\\(|>\\)"                    #Xe135)
              ("\\(\\^=\\)"                  #Xe136)
              ;; ("\\(\\$>\\)"                  #Xe137)
              ;; ++                          #Xe138
              ("\\(\\+\\+\\)"                #Xe138)
              ;; +++                         #Xe139
              ("\\(\\+\\+\\+\\)"             #Xe139)
              ;; ("\\(\\+>\\)"                  #Xe13a)
              ("\\(=:=\\)"                   #Xe13b)
              ("[^!/]\\(==\\)[^>]"           #Xe13c)
              ("\\(===\\)"                   #Xe13d)
              ("\\(==>\\)"                   #Xe13e)
              ("[^=]\\(=>\\)"                #Xe13f)
              ("\\(=>>\\)"                   #Xe140)
              ("\\(<=\\)"                    #Xe141)
              ("\\(=<<\\)"                   #Xe142)
              ("\\(=/=\\)"                   #Xe143)
              ("\\(>-\\)"                    #Xe144)
              ("\\(>=\\)"                    #Xe145)
              ("\\(>=>\\)"                   #Xe146)
              ("[^-=]\\(>>\\)"               #Xe147)
              ("\\(>>-\\)"                   #Xe148)
              ("\\(>>=\\)"                   #Xe149)
              ("\\(>>>\\)"                   #Xe14a)
              ;; <*                          #Xe
              ("\\(<\\*\\)"                  #Xe14b)
              ;; *>                          #Xe
              ("\\(<\\*>\\)"                 #Xe14c)
              ("\\(<|\\)"                    #Xe14d)
              ("\\(<|>\\)"                   #Xe14e)
              ;; ("\\(<\\$\\)"                  #Xe14f)
              ;; ("\\(<\\$>\\)"                 #Xe150)
              ;; ("\\(<!--\\)"                  #Xe151)
              ("\\(<-\\)"                    #Xe152)
              ("\\(<--\\)"                   #Xe153)
              ("\\(<->\\)"                   #Xe154)
              ;; ("\\(<\\+\\)"                  #Xe155)
              ;; ("\\(<\\+>\\)"                 #Xe156)
              ("\\(<=\\)"                    #Xe157)
              ("\\(<==\\)"                   #Xe158)
              ("\\(<=>\\)"                   #Xe159)
              ("\\(<=<\\)"                   #Xe15a)
              ;; ("\\(<>\\)"                    #Xe15b)
              ;; -<<                         #Xe
              ;; =<<                         #Xe
              ;; ("[^-=]\\(<<\\)"               #Xe15c)
              ("\\(<<-\\)"                   #Xe15d)
              ("\\(<<=\\)"                   #Xe15e)
              ("\\(<<<\\)"                   #Xe15f)
              ("\\(<~\\)"                    #Xe160)
              ("\\(<~~\\)"                   #Xe161)
              ;; ("\\(</\\)"                    #Xe162)
              ;; ("\\(</>\\)"                   #Xe163)
              ;; ("\\(~@\\)"                    #Xe164)
              ("\\(~-\\)"                    #Xe165)
              ("\\(~=\\)"                    #Xe166)
              ("\\(~>\\)"                    #Xe167)
              ("[^<]\\(~~\\)"                #Xe168)
              ("\\(~~>\\)"                   #Xe169)
              ;; ("\\(%%\\)"                    #Xe16a)
              ;; ("[^:=]\\(:\\)[^:=]"           #Xe16c)
              ;; ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
              ;; (*                          #Xe
              ("\\s(\\(\\*\\)[ \r\t\n]"      #Xe16f))))

  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords nil fira-code-font-lock-keywords-alist t)
              (when (eq 'gnu/linux system-type)
                (font-lock-add-keywords
                 nil
                 '(("[ \t]\\([-+]?=\\)[ \t]"
                    (1 '((:weight normal)) prepend))
                   ("\\s(\\(=\\)[ \t]"
                    (1 '((:weight normal)) prepend)))
                 t)))
            :append))

(use-package sh-script
  :defer t
  :config
  (let* ((symbol "[@?_0-9a-zA-Z]+")
         (symbol_ (concat "\\(?:\\$" symbol "\\|\\${" symbol "}\\)"))
         (whitespace "[ \r\t]")
         (whitespace+ (concat whitespace "+"))
         (whitespace* (concat whitespace "*"))
         (assigment "=[^=]"))
    (font-lock-add-keywords
     'sh-mode
     `((,(concat "^" whitespace+ "\\(local\\)" whitespace+ "\\(" symbol "\\)" assigment)
        (1 'font-lock-keyword-face)
        (2 'font-lock-variable-name-face))))
    (font-lock-add-keywords
     'sh-mode
     `((,(concat "\\(" symbol_ "\\)")
        (1 (let* ((face (plist-get (text-properties-at (1- (match-beginning 0))) 'face))
                  (face-lst (if (listp face) face (list face))))
             (when (or (memq 'font-lock-comment-face face-lst)
                       (memq 'font-lock-string-face  face-lst))
               face))
           t)))
     :append)))

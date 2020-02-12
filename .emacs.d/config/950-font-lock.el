(setq font-lock--anchor-beg-point nil
      font-lock--local-limit nil)
(make-local-variable 'font-lock--local-limit)
(make-local-variable 'font-lock--anchor-beg-point)

(defn fake-match-2 ()
  "TODO"
  (-repeat 2 (point-min-marker)))

(defn fake-match-4 ()
  "TODO"
  (-repeat 4 (point-min-marker)))

(use-package cc-mode
  :defer t
  :config
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (when (member major-mode '(c-mode c++-mode))
       (font-lock-add-keywords
        nil
        `(("\\([&|*]\\|::\\|[-=]>\\)"
           (1 'shadow)))
        :append)))))

(use-package elixir-mode
  :defer t
  :config
  (defface elixir-argument-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "TODO")

  (let* ((symbol "[_0-9a-zA-Z?!]+")
         (symbol-fn (byte-compile
                     (-partial
                      (lambda (symbol limit)
                        (let ((found-p nil))
                          (while (and (re-search-forward (concat "\\_<\\(" symbol "\\)\\_>") limit t)
                                      (not found-p))
                            (unless (equal ?^ (char-before (match-beginning 1)))
                              (setq found-p t)))
                          found-p))
                      symbol))))
    ;; prepend rules
    (font-lock-add-keywords
     'elixir-mode
     `(("::[ ]?binary"
        (0 'font-lock-builtin-face))))
    ;; append rules
    (font-lock-add-keywords
     'elixir-mode
     `(("\\(?:\\_<\\|\\s(\\)\\(\\?.\\)"
        (1 'font-lock-negation-char-face))
       ;; Highlighting variables
       (,(concat "\\(?:^\\s-*\\|\\(?:for\\|with\\)\\s-+\\)\\(" symbol "\\)\\s-+\\(<-\\|->\\)")
        (1 'font-lock-variable-name-face))
       ;; Highlighting pattern matching variables
       ("\\(\\(?:\\[\\|%?{\\).+?\\(?:\\]\\|}\\)\\)\\s-*="
        (,symbol-fn
         (progn
           (goto-char (setq font-lock--anchor-beg-point (match-beginning 0)))
           (match-end 1))
         (goto-char font-lock--anchor-beg-point)
         (1 'font-lock-variable-name-face)))
       ("\\(\\(?:\\[\\|%?{\\).+?\\(?:\\]\\|}\\)\\)\\(?:[ \t\r\n]+when\\s-+.+\\)?\\s-*\\(<-\\|->\\)"
        (,symbol-fn
         (progn
           (goto-char (setq font-lock--anchor-beg-point (match-beginning 0)))
           (match-end 1))
         (goto-char font-lock--anchor-beg-point)
         (1 'font-lock-variable-name-face)))
       ;; Highlighting argument variables
       ("\\<\\(?:fn\\)\\s-+\\(.+\\)\\s-+->"
        (,symbol-fn
         (progn
           (setq font-lock--anchor-beg-point (match-beginning 0))
           (goto-char (match-beginning 1))
           (match-end 1))
         (goto-char font-lock--anchor-beg-point)
         (1 'elixir-argument-name-face)))
       (,(concat "\\<\\(?:defp?\\|defmacrop?\\)\\s-+" symbol "\\s-*(")
        (,symbol-fn
         (save-excursion
           (setq font-lock--anchor-beg-point (point))
           (up-list)
           (point))
         (goto-char font-lock--anchor-beg-point)
         (1 'elixir-argument-name-face)))
       ("\\(<<\\|>>\\|[&%]\\|\\s(\\|\\s)\\)"
        (1 'shadow))
       ("\\(/\\)[0-9]"
        (1 'shadow)))
     :append)))

(use-package cperl-mode
  :defer t
  :config
  (defface cperl-less-important-keyword-face
    `((t (:inherit font-lock-keyword-face :weight ,(face-attribute 'default :weight))))
    "TODO")

  (let ((unimportant-kws '("bless" "defined" "delete" "exists" "grep" "join" "lc" "map" "push" "ref" "splice")))
   (font-lock-add-keywords
    'cperl-mode
    `((,(regexp-opt unimportant-kws 'symbols)
       1 'cperl-less-important-keyword-face))))
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
        (1 'font-lock-type-face))
       ("\\(?:\\>\\|\\_>\\|\\s\"\\|\\s)\\)\\s-*\\(::+\\|[-=]>\\|/\\)\\s-*\\(?:\\<\\|\\_<\\|\\s\"\\|\\s(\\|\\$\\|\\\\\\)"
        (1 'shadow))
       ("\\([&|*]\\|::\\|[-=]>\\)"
        (1 'shadow))
       ("\\([*@$%]+\\)\\(?:[:_0-9a-zA-Z]\\|\\s(\\)"
        (1 'shadow))))
   :append))

(use-package clojure-mode
  :defer t
  :config
  (defface clojure-defining-spec-face
    `((t (:inherit (clojure-keyword-face))))
    "Face used to font-lock Clojure defining Spec")
  (defface clojure-side-effect-face
    `((t (:inherit (italic font-lock-variable-name-face))))
    "Face used to font-lock Clojure side-effect indicator.")
  (defface clojure-important-keywords-face
    '((t (:inherit (italic font-lock-keyword-face))))
    "Face used to font-lock Clojure important keywords.")
  (defface clojure-special-variable-name-face
    `((t (:inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
    "Face used to font-lock Clojure special variable name.")
  (defface clojure-local-binding-variable-name-face
    `((t (:inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
    "Face used to font-lock Clojure local binding variable name.")
  (defface clojure-local-binding-variable-name-warning-face
    '((t (:inherit (italic clojure-local-binding-variable-name-face))))
    "TODO")
  (defface clojure-fn-parameter-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Clojure parameter.")
  (defface clojure-fn-parameter-warning-face
    '((t (:inherit (italic clojure-fn-parameter-face))))
    "TODO")
  (defface clojure-semi-function-name-face
    `((t (:inherit font-lock-function-name-face :weight ,(face-attribute 'default :weight))))
    "Face used to font-lock Clojure OOP style functions.")
  (defface clojure-cond-condtion-face
    '((t (:inherit italic)))
    "Face used to font-lock Clojure conditions in `cond' form.")
  (defface clojure-if-true-face
    '((t (:inherit italic)))
    "Face used to font-lock Clojure `if' true form.")
  (defface clojure-define-type-face
    '((t (:inherit (font-lock-type-face))))
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

  (defn clojure-forward-sexp (&optional n)
    "TODO"
    (or n (setq n 1))
    (while (not (zerop n))
      (if (> n 0) (clojure-skip  1 :comment :ignored-form :tagged-literal))
      (forward-sexp (if (< 0 n) 1 -1))
      (if (< n 0) (clojure-skip -1 :comment :ignored-form :tagged-literal))
      (setq n (funcall (if (< 0 n) '1- '1+) n))))

  (setq font-lock--anchor-beg-point nil)
  (setq clojure-binding-form--recursive-point nil)
  (setq clojure-binding-form--recursive-limit nil)
  (setq clojure-oop-kw--str nil)
  (setq clojure-oop-fn-form--points nil)
  (setq clojure-oop-fn-recursive--point nil)
  (setq clojure-oop-fn-recursive--limit nil)
  (setq clojure-fn-form--method? nil)
  (setq clojure-fn-form--multi-arity? nil)
  (setq clojure-fn-recursive--point nil)
  (setq clojure-fn-recursive--limit nil)
  (make-local-variable 'clojure-binding-form--recursive-point)
  (make-local-variable 'clojure-binding-form--recursive-limit)
  (make-local-variable 'clojure-oop-kw--str)
  (make-local-variable 'clojure-oop-fn-form--points)
  (make-local-variable 'clojure-oop-fn-recursive--point)
  (make-local-variable 'clojure-oop-fn-recursive--limit)
  (make-local-variable 'clojure-fn-form--method?)
  (make-local-variable 'clojure-fn-form--multi-arity?)
  (make-local-variable 'clojure-fn-recursive--point)
  (make-local-variable 'clojure-fn-recursive--limit)

  (let* ((whitespace "[ \r\t\n]")
         (whitespace+ (concat whitespace "+"))
         (whitespace* (concat whitespace "*"))
         (symbol  clojure--sym-regexp)
         (symbol? (concat "\\(?:" symbol "\\)?"))
         (namespace  (concat "\\(?:" symbol "/\\)"))
         (namespace? (concat namespace "?"))
         (meta? "\\(?:\\(?:\\^{[^^]+}\\|\\^:?\\sw+\\)[ \r\n\t]+\\)?")
         (core-ns  (concat (regexp-opt '("clojure.core" "cljs.core" "core") nil) "/"))
         (core-ns? (concat "\\(?:" core-ns "\\)?"))
         (if-kw   (regexp-opt '("if" "if-some" "if-let" "if-not")))
         (oop-kw  (regexp-opt '("definterface" "defprotocol" "defrecord" "deftype" "extend-protocol" "extend-type" "proxy" "reify")))
         (def-kw  (regexp-opt '("defmacro" "defn" "defn-" "defmethod" "defrecord" "deftype") t))
         (cond-kw (regexp-opt '("case" "cond" "condp" "cond->" "cond->>"
                                "for" "if" "if-let" "if-not" "recur" "throw" "when"
                                "loop" "when-let" "when-not" "while") t))
         (custom-kw (regexp-opt '("go-loop" "with-hard-redefs") t)))

    (defconst clojure-font-lock-keywords
      `(;; Binding forms
        (,(concat "(" core-ns? (regexp-opt clojure--binding-forms) "[ \r\t\n]+\\[")
         ;; Normal bindings
         (,(byte-compile
            (-partial
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
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)")))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'clojure-local-binding-variable-name-face))

         ;; Destructuring bindings
         (,(byte-compile
            (-partial
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
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>")))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (setq clojure-binding-form--recursive-point nil)
              (setq clojure-binding-form--recursive-limit nil)
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'clojure-local-binding-variable-name-face)))

        ;; OOP style function forms & letfn
        (,(concat "(" core-ns? "\\(" oop-kw whitespace+ meta? "\\|" "letfn" whitespace+ "\\[" "\\)")
         ;; highlighting definition type symbol
         (,(byte-compile
            (-partial
             (lambda (symbol limit)
               (unless font-lock--skip
                 (and (string-match-p "^def" clojure-oop-kw--str)
                      (re-search-forward symbol limit t))))
             (concat "\\(" symbol "\\)")))
          (save-excursion
            (setq clojure-oop-kw--str (match-string-no-properties 1))
            (setq font-lock--anchor-beg-point (point))
            (condition-case nil
                (clojure-forward-sexp)
              (error (setq font-lock--skip t)))
            (point))
          (goto-char font-lock--anchor-beg-point)
          (0 'clojure-define-type-face))
         ;; highlighting OOP fn name
         (,(byte-compile
            (-partial
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
             (concat symbol "\\>")))
          (save-excursion
            (setq clojure-oop-fn-form--points nil)
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-semi-function-name-face))

         ;; Highlighting OOP fn parameters
         (,(byte-compile
            (-partial
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
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>")))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (setq clojure-oop-fn-recursive--point nil)
              (setq clojure-oop-fn-recursive--limit nil)
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'clojure-fn-parameter-face)))

        ;; Clojure definition keywords - (DEFINITION-KEYWORD symbol ...)
        (,(concat "(" core-ns? def-kw "\\>" whitespace+ meta? "\\(" symbol "*?\\)\\(!*\\)\\>")
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face)
         (3 'clojure-side-effect-face t)

         ;; fn parameters highlight
         (,(byte-compile
            (-partial
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
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>")))
          (if (in-comment?)
              (setq font-lock--skip t)
            (setq font-lock--skip nil)
            (setq font-lock--anchor-beg-point (point))
            (setq clojure-fn-form--method? (string-match-p "defmethod" (match-string 1)))
            (setq clojure-fn-form--multi-arity? nil)
            (setq clojure-fn-recursive--point nil)
            (setq clojure-fn-recursive--limit nil)
            (save-excursion
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'clojure-fn-parameter-face)))

        ;; %
        ("\\<%[&1-9]?\\>"
         (0 'clojure-special-variable-name-face))

        ;; Dynamic variables - *variable* or @*variable*
        (,(concat "\\_<@?\\(\\*[^" clojure--sym-forbidden-rest-chars "*]*\\*\\)\\_>")
         (1 'clojure-special-variable-name-face))

        ;; #js{...}
        (,(concat "\\(#js\\)" whitespace* "\\s(")
         (1 'font-lock-builtin-face))

        ;; Interop - (..symbol ...)
        (,(concat "(\\(\\.\\.\\) " symbol)
         (1 'clojure-interop-method-face)
         ;; -symbol
         (,(concat "\\<\\(-\\)" symbol)
          (save-excursion
            (up-list)
            (point))
          nil
          (1 'clojure-interop-method-face)))

        ;; Interop - (.symbol ...)  or (.-symbol ...)
        (,(concat "(\\(\\.-?\\)" symbol)
         (1 'clojure-interop-method-face))

        ;; Interop new - (symbol. ...)
        (,(concat "(" symbol "\\(\\.\\)" whitespace)
         (1 'clojure-interop-method-face))

        ;; Built-in binding and flow of control forms
        (,(concat "(" core-ns? cond-kw "\\(?:)\\|" whitespace "\\)")
         (1 'clojure-important-keywords-face))

        ;; Namespaced keyword - ::namespace/keyword
        (,(concat "::\\(" symbol "\\)\\(/\\)" symbol "\\>")
         (1 'font-lock-type-face)
         (2 'shadow))

        ;; Auto-gensym variable - variable#
        (,(concat "[0-9A-Za-z?]\\(#\\)")
         (1 'shadow))

        ;; Dereference, Unquote - @symbol or ~symbol
        (,(concat "\\(~\\|@\\)[*0-9A-Za-z]")
         (1 'shadow))

        ;; Function definition (anything that starts with def and is not listed above)
        (,(concat "(" core-ns? "\\(" def-kw "\\)\\>"
                  whitespace* meta?
                  "\\(" symbol "?\\)")
         (1 'font-lock-keyword-face)
         (2 'font-lock-variable-name-face nil t))

        ;; Type definition
        (,(concat "(" core-ns? "\\(" (regexp-opt '("defstruct" "deftype" "defprotocol" "defrecord")) "\\)\\>"
                  whitespace*
                  meta?
                  "\\(" symbol "\\)?")
         (1 'font-lock-keyword-face)
         (2 'font-lock-type-face nil t))

        ;; Top-level variable definition
        (,(concat "(" namespace?
                  "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                  whitespace*
                  meta?
                  "\\(" symbol "\\)?")
         (1 'font-lock-keyword-face)
         (2 'font-lock-variable-name-face nil t))

        ;; (fn name? args ...)
        (,(concat "(" core-ns? "\\(fn\\)"
                  whitespace+
                  meta?
                  "\\(" symbol "\\)?" )
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face nil t)

         ;; fn parameters highlight
         (,(byte-compile
            (-partial
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
             (concat meta? "\\_<" namespace? "\\(" symbol "\\)\\>")))
          (if (in-comment?)
              (setq font-lock--skip t)
            (setq font-lock--skip nil)
            (setq font-lock--anchor-beg-point (point))
            (setq clojure-fn-form--multi-arity? nil)
            (setq clojure-fn-recursive--point nil)
            (setq clojure-fn-recursive--limit nil)
            (save-excursion
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'clojure-fn-parameter-face)))

        ;; (default ....) or (namespace/default ...)
        (,(concat "(" namespace? "\\(default\\)\\>")
         (1 nil))

        ;; Spec definition - (def ::keyword ...) or (def :namespace/keyword ...)
        (,(concat "(" namespace? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                  whitespace+
                  meta?
                  "\\(?::" namespace? "\\|::\\)\\(" symbol "\\)\\>")
         (1 'font-lock-keyword-face)
         (2 'clojure-defining-spec-face))

        ;; Special forms
        (,(concat
           "(" (regexp-opt '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
                             "recur" "throw" "try" "catch" "finally"
                             "set!" "new" "."
                             "monitor-enter" "monitor-exit" "quote") t)
           "\\>")
         (1 'font-lock-keyword-face))

        ;; Built-in binding and flow of control forms
        (,(concat
           "(" core-ns?
           (regexp-opt '("letfn" "case" "cond" "cond->" "cond->>" "condp"
                         "for" "when" "when-not" "when-let" "when-first" "when-some"
                         ".." "doto"
                         "dosync" "doseq" "dotimes" "dorun" "doall"
                         "ns" "in-ns"
                         "with-open" "with-local-vars" "binding"
                         "with-redefs" "with-redefs-fn"
                         "declare") t)
           "\\>")
         (1 'font-lock-keyword-face))

        ;; Macros similar to let, when, and while
        (,(rx symbol-start
              (or "let" "when" "while") "-"
              (1+ (or (syntax word) (syntax symbol)))
              symbol-end)
         (0 'font-lock-keyword-face))

        ;; Built-in variable
        (,(concat
           "\\<"
           (regexp-opt '("*1" "*2" "*3" "*agent*"
                         "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
                         "*command-line-args*" "*compile-files*"
                         "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
                         "*e" "*err*" "*file*" "*flush-on-newline*"
                         "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
                         "*print-dup*" "*print-length*" "*print-level*"
                         "*print-meta*" "*print-readably*"
                         "*read-eval*" "*source-path*"
                         "*unchecked-math*"
                         "*use-context-classloader*" "*warn-on-reflection*")
                       t)
           "\\>")
         (0 'font-lock-builtin-face))

        ;; Global constants - nil, true, false
        (,(concat "\\<" (regexp-opt '("true" "false" "nil") t) "\\>")
         (0 'font-lock-constant-face))

        ;; character literals - \1, \a, \newline, \u0000
        ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)"
         (0 'clojure-character-face))

        ;; Namespace definitions - (ns foo.bar)
        (,(concat "(\\<ns\\>" whitespace* meta? "\\(" symbol "\\)")
         (1 'font-lock-type-face))

        ;; keywords - {:oneword/ve/yCom|pLex.stu-ff 0}
        (,(concat "\\(::?\\)\\(" symbol "?\\)\\(/\\)\\(" symbol "\\)")
         (1 'clojure-keyword-face)
         (2 'font-lock-type-face)
         (3 'shadow)
         (4 'clojure-keyword-face))
        (,(concat "\\(::?" symbol "\\)")
         (1 'clojure-keyword-face))

        ;; clojure symbols not matched by the previous regexps; influences CIDER's
        ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
        (,(concat "\\(" symbol "?\\)\\(/\\)\\(" symbol "\\)")
         (1 'font-lock-type-face)
         ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
         ;; CDSH seems to kick in only for functions and variables referenced w/o
         ;; writing their namespaces.
         (2 'shadow)
         (3 nil))
        (,(concat "\\(" symbol "\\)")
         ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
         (1 nil))

        ;; #_ and (comment ...) macros.
        (clojure--search-comment-macro
         (1 font-lock-comment-face t))

        ;; Highlight `code` marks, just like `elisp'.
        (,(rx "`" (group-n 1 (optional "#'") (+ (or (syntax symbol) (syntax word)))) "`")
         (1 'font-lock-constant-face prepend))

        ;; Highlight escaped characters in strings.
        (clojure-font-lock-escaped-chars
         (0 'bold prepend))

        ;; Highlight grouping constructs in regular expressions
        (clojure-font-lock-regexp-groups
         (1 'font-lock-regexp-grouping-construct prepend))

        ;; Highlight condtions in `cond' form.
        (,(concat "(" core-ns? "\\(cond\\(?:->>?\\)?\\)[ \r\t\n]+")
         (,(byte-compile
            (lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (when (> limit (point))
                  (clojure-skip :comment :ignored-form)
                  (set-match-data (list (point-marker) (progn (forward-sexp) (point-marker))))
                  (clojure-forward-sexp)
                  t))))
          (prog1 (save-excursion
                   (if (in-comment?)
                       (setq font-lock--skip t)
                     (setq font-lock--skip nil)
                     (setq font-lock--anchor-beg-point (point))
                     (safe-up-list-1)
                     (point)))
            (when (string-match-p "->>?" (match-string 1))
              (condition-case nil
                  (clojure-forward-sexp)
                (error (setq font-lock--skip t)))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-cond-condtion-face prepend)))

        ;; Highlight 'true' clause in `if' form.
        (,(concat "(" core-ns? if-kw whitespace+)
         (,(byte-compile
            (lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (when (> limit (point))
                  (clojure-forward-sexp)
                  (set-match-data (list (progn (clojure-skip :comment :ignored-form) (point-marker))
                                        (progn (clojure-forward-sexp) (point-marker))))
                  (clojure-forward-sexp)
                  t))))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (safe-up-list-1)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-if-true-face append)))

        ;; CSS
        (,(concat "(" namespace? "css" whitespace)
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
         ,(byte-compile
           (-partial
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
            (concat "(" meta? symbol "\\>")))
         (save-excursion
           (if (in-comment?)
               (setq font-lock--skip t)
             (setq font-lock--skip nil)
             (setq font-lock--anchor-beg-point (point))
             (safe-up-list-1)
             (point)))
         (if font-lock--skip
             (end-of-line)
           (goto-char font-lock--anchor-beg-point))
         (0 'font-lock-doc-face t))

        ;; Metadata
        (,(concat "\\(?:" whitespace "\\|[([{]\\)\\^[:A-Za-z{]")
         (,(byte-compile
            (lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (let ((start (progn
                               (backward-char 1)
                               (point-marker))))
                  (goto-char limit)
                  (set-match-data (list start (point-marker)))
                  t))))
          (save-excursion
            (if (in-comment?)
                (setq font-lock--skip t)
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (backward-char 1)
              (clojure-forward-sexp)
              (point)))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-meta-face t)))

        ;; side-effect
        (,(concat symbol "?\\(!+\\)\\>")
         (1 'clojure-side-effect-face append))

        ;; Interop type name
        (,(concat "\\(" symbol "\\(\\." symbol "\\)+\\)")
         (1 'font-lock-type-face nil))
        (,(concat "\\<\\(\\([A-Z]" symbol "\\)\\)\\>")
         (1 'font-lock-type-face nil))

        ;; Custom keywords
        (,(concat "(" namespace? custom-kw)
         (1 'font-lock-keyword-face)))
      "Default expressions to highlight in Clojure mode.")))

(use-package elisp-mode
  :defer t
  :config
  (defface lisp-local-binding-variable-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Lisp local binding variable name.")

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
          (1 'default nil))
         ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
          (1 'font-lock-constant-face))
         ("(\\(assert\\)"
          (1 'font-lock-variable-name-face))
         (,(concat "(defn" whitespace+ "\\(" symbol "\\)")
          (1 'font-lock-function-name-face))
         ;; local variables
         (,(concat "(\\(lexical-\\|-\\)?let\\*?" whitespace+ "(")
          (,(byte-compile
             (-partial
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
              (concat "(\\(" symbol "\\)" whitespace+)))
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
         (,(concat "\\(defn\\|defun\\|lambda\\)" whitespace+ "\\(" symbol whitespace+ "\\)?(")
          (,(byte-compile
             (-partial
              (lambda (symbol limit)
                (ignore-errors
                  (when font-lock--skip
                    (error ""))
                  (when (re-search-forward symbol limit t)
                    (when (string-match-p "^&" (match-string 1))
                      (set-match-data (fake-match-4)))
                    t)))
              (concat "\\(" symbol "\\)\\>")))
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
          (1 'lisp-local-binding-variable-name-face)))))
    (font-lock-add-keywords
     mode
     `(("\\_<\\(\\?.\\)"
        (1 'font-lock-string-face)))
     :append)))

(use-package go-mode
  :defer t
  :config
  (defface go-argument-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "TODO")

  ;; prepend
  (font-lock-add-keywords
   'go-mode
   (let* ((symbol  "\\_<[_A-Za-z][_0-9A-Za-z]*\\_>")
          (type-prefix "\\(?:\\(?:\\[\\]\\|map\\[[_.0-9A-Za-z]+\\]\\)\\*?\\)")
          (type-suffix "\\(?:{[^}\r\n]*}\\)")
          (user-type "[_.0-9A-Za-z]+")
          (system-type (regexp-opt '("bool" "string"
                                     "int" "int8" "int16" "int32" "int64"
                                     "uint" "uint8" "uint16" "uint32" "uint64"
                                     "byte" "rune" "uintptr"
                                     "float32" "float64"
                                     "complex64" "complex128")))
          (type (concat type-prefix "?\\(?:" user-type "\\|" system-type "\\)" type-suffix "?"))
          (whitespace "[ \t]")
          (whitespace* (concat whitespace "*"))
          (whitespace+ (concat whitespace "+")))
     `(;; highlight strucut
       (,(concat "\\_<struct" whitespace* "{")
        (,(-partial
           (lambda (symbol type-prefix whitespace* whitespace+ limit)
             (comment-forward (point-max))
             (while (and (re-search-forward (concat symbol "\\(,\\)?" whitespace+) limit t)
                         (string= "," (match-string 1))))
             (re-search-forward (concat whitespace* type-prefix "?\\(" symbol "\\)") limit t))
           symbol type-prefix whitespace* whitespace+)
         (save-excursion
           (setq font-lock--anchor-beg-point (point))
           (up-list)
           (point))
         (goto-char font-lock--anchor-beg-point)
         (1 'font-lock-type-face)))
       ;; highlight variables
       (,(concat "\\_<var" whitespace+ "\\(" symbol "\\)" whitespace+ type-prefix "?\\(" symbol "\\)")
        (1 'font-lock-variable-name-face)
        (2 'font-lock-type-face))
       (,(concat "\\_<var" whitespace+ "\\(" symbol "\\)")
        (1 'font-lock-variable-name-face))
       (,(concat symbol "\\(?:," whitespace* symbol "\\)*" whitespace* "\\(:=\\)")
        (,symbol
         (progn
           (goto-char (setq font-lock--anchor-beg-point (match-beginning 0)))
           (match-beginning 1))
         (goto-char font-lock--anchor-beg-point)
         (0 'font-lock-variable-name-face)))
       ;; highlight arguments
       (,(concat "\\(\\_<func\\>\\)" whitespace* "\\(([^()]+)\\)?" whitespace* symbol whitespace* "\\(([^()]+)\\)?")
        (,(byte-compile
           (-partial
            (lambda (symbol type limit)
              (when (null font-lock--local-limit)
                (skip-chars-forward "^(" limit)
                (when (< (point) limit)
                  (save-excursion
                    (forward-char)
                    (up-list)
                    (setq font-lock--local-limit (point)))))
              (when (and font-lock--local-limit
                         (re-search-forward type font-lock--local-limit t))
                (unless (looking-at-p ",\\|\\s)")
                  (skip-chars-forward "^," font-lock--local-limit)
                  (when (<= font-lock--local-limit (point))
                    (setq font-lock--local-limit nil)))
                t))
            symbol type))
         (progn
           (setq font-lock--anchor-beg-point (match-end 1)
                 font-lock--local-limit nil)
           (prog1 (point)
             (goto-char font-lock--anchor-beg-point)))
         (goto-char font-lock--anchor-beg-point)
         (0 'go-argument-name-face)))))))

(use-package js
  :defer t
  :config
  (font-lock-add-keywords
   'js-mode
   `(("\\(?:async\\|export\\)\\s-+function\\s-+\\([_0-9A-Za-z]+\\)\\>"
      (1 'font-lock-function-name-face)))))

(use-package php-mode
  :defer t
  :config
  (defface php-passive-assign-variable-face
    '((t (:inherit font-lock-variable-name-face)))
    "TODO")

  (let* ((symbol "\\$[_0-9a-zA-Z]+")
         (whitespace "[ \r\t\n]")
         (whitespace+ (concat whitespace "+"))
         (whitespace* (concat whitespace "*"))
         (assigment (concat whitespace* "\\s-=\\s-"))
         (pre `(progn
                 (setq font-lock--anchor-beg-point (point))
                 (save-match-data
                   (if (re-search-forward (concat ,whitespace+ "\\?>") nil t)
                       (prog1 (point)
                         (goto-char font-lock--anchor-beg-point))
                     (point-max)))))
         (post '(goto-char font-lock--anchor-beg-point)))
    (defconst php-font-lock-keywords-3
      `((,(concat "\\(?:^\\|" whitespace+ "\\)\\(\\?>\\)")
         (1 'php-php-tag))
        (,(concat "\\(<\\?php\\)" whitespace+)
         (1 'php-php-tag))
        (,(caar php-phpdoc-font-lock-keywords))
        (,(concat "\\<function" whitespace+ "[_0-9a-z-A-Z]*" whitespace* "(")
         (,(concat "\\(" symbol "\\)")
          (save-excursion
            (setq font-lock--anchor-beg-point (point))
            (up-list)
            (point))
          (goto-char font-lock--anchor-beg-point)
          (1 'php-passive-assign-variable-face t)))
        (,(concat "\\(" symbol "\\)\\(?:\\[.*?\\]\\)?" assigment)
         (1 'font-lock-variable-name-face t))
        (,(concat symbol "->\\([_0-9a-zA-Z]+\\)\\(?:\\[.*?\\]\\)?" assigment)
         (1 'font-lock-variable-name-face t))
        ("$\\(this\\|that\\)\\_>"
         (1 'php-$this))
        ("\\<function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
         (1 'php-function-name))
        ("\\b\\(array\\|callable\\)\\s-+&?\\$"
         (1 'font-lock-type-face))
        (")\\s-*:\\s-*\\??\\(array\\|callable\\)\\b"
         (1 'font-lock-type-face))
        ("(\\(array\\))"
         (1 'font-lock-type-face))
        ("\\(\\([a-zA-Z0-9_]+\\\\\\)+[a-zA-Z0-9_]+\\|\\(\\\\[a-zA-Z0-9_]+\\)+\\)[^:a-zA-Z0-9_\\\\]"
         (1 'font-lock-type-face))
        ("\\(\\([a-zA-Z0-9_]+\\\\\\)+[a-zA-Z0-9_]+\\|\\(\\\\[a-zA-Z0-9_]+\\)+\\)::"
         (1 'php-constant))
        ("\\sw+\\(::\\)\\(class\\)\\b"
         (1 'shadow)
         (2 'php-constant))
        ,@(c-lang-const c-matchers-3 php)
        ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>"
         (1 'php-constant))
        ("\\(\\sw+\\)\\(::\\)"
         (1 'php-constant)
         (2 'shadow))
        (,(concat "\\<as\\s-+\\(" symbol "\\)\\_>")
         (1 'php-passive-assign-variable-face))
        (,(concat (regexp-opt (c-lang-const c-class-decl-kwds php)) " \\(\\sw+\\)")
         (1 'font-lock-type-face))
        ("function.+:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+"
         (1 'font-lock-type-face))
        (")\\s-*:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+\\s-*\\(?:\{\\|;\\)"
         (1 'font-lock-type-face))
        ("\\?\\(\\(:?\\sw\\|\\s_\\)+\\)\\s-+\\$"
         (1 'font-lock-type-face))
        ("function.+:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)"
         (1 'font-lock-type-face))
        (")\\s-*:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\\(?:\{\\|;\\)"
         (1 'font-lock-type-face))
       ("\\(?:^\\|\\>\\|\\_>\\|\\s\"\\|\\s)\\)\\s-*\\(::+\\|[-=]>\\|/\\)\\s-*\\(?:\\<\\|\\_<\\|\\s\"\\|\\s(\\)"
        (1 'shadow)))))
  (setq php-font-lock-keywords php-font-lock-keywords-3)
  (font-lock-add-keywords
   'php-mode
   '(("\\([$]\\)"
      (1 'shadow append)))
   :append))

(use-package prog-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              "TODO"
              (font-lock-add-keywords
               nil
               '(("\\([.,;]\\|\\s(\\|\\s)\\)"
                  (1 'shadow append)))
               :append))
            :append))

(use-package rpm-spec-mode
  :defer t
  :config
  (font-lock-add-keywords
   'rpm-spec-mode
   '(("\\(#?'\\|[.,/<>]\\|\\s(\\|\\s)\\)"
      (1 'shadow append)))
   :append))

(use-package rust-mode
  :defer t
  :config
  (defface rust-self-var-face
    `((t (:inherit font-lock-keyword-face :weight ,(face-attribute 'default :weight))))
    "TODO")

  (font-lock-add-keywords
   'rust-mode
   `(("\\_<\\(self\\)\\."
      (1 'rust-self-var-face))
     ("^\\s-*\\(use\\)\\s-+\\([_:0-9A-Za-z]+\\)"
      (1 'font-lock-keyword-face)
      (2 'font-lock-constant-face))))
  (font-lock-add-keywords
   'rust-mode
   `(("\\_>:\\s-+\\(?:[*&]mut\\s-+\\)?\\([_0-9A-Za-z]+\\)\\_>"
      (1 'font-lock-type-face))
     (")\\s-+->\\s-+\\(?:[*&]mut\\s-+\\)?\\(?:[_0-9A-Za-z]::\\)*\\([_0-9A-Za-z]+\\)\\_>"
      (1 'font-lock-type-face))
     ("\\s-+as\\s-+\\(?:[*&]mut\\s-+\\)?\\(?:[_0-9A-Za-z]::\\)*\\([_0-9A-Za-z]+\\)\\_>"
      (1 'font-lock-type-face))
     ("\\(|\\)\\([^\r\n|]+\\)\\(|\\)"
      (1 'shadow)
      (3 'shadow)
      ("\\_<[_0-9A-Za-z]+\\_>"
       (progn
         (goto-char (setq font-lock--anchor-beg-point (match-beginning 2)))
         (match-end 2))
       (goto-char font-lock--anchor-beg-point)
       (0 'font-lock-variable-name-face)))
     ("\\([&*:']\\|::\\|[-=]>\\)"
      (1 'shadow))
     ("\\(<+\\)\\(?:[^ \t\r\n]\\|\\s-*$\\)"
      (1 'shadow))
     ("\\(?:^\\s-*\\|[^ \t\r\n]\\)\\(>+\\)"
      (1 'shadow))
     ("\\_<\\(_\\)\\_>"
      (1 'shadow))
     ("^use\\s-+\\(?:[_0-9A-Za-z]+::\\)+\\s-*{"
      ("\\_<[A-Za-z]+\\_>"
       (save-excursion
         (setq font-lock--skip nil)
         (safe-up-list-1)
         (point))
       nil
       (0 font-lock-constant-face t))))
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
     `((,(concat "\\(\\$\\)\\(" symbol "\\|{" symbol "}\\)")
        (1 'shadow)
        (2 (let* ((face (plist-get (text-properties-at (1- (match-beginning 0))) 'face))
                  (face-lst (if (listp face) face (list face))))
             (when (or (memq 'font-lock-comment-face face-lst)
                       (memq 'font-lock-string-face  face-lst))
               face))
           t))
       ("\\([&|<>]\\)"
        (1 'shadow)))
     :append)))

(use-package web-mode
  :defer t
  :config
  (let* ((pairs (->> web-mode-engines-auto-pairs
                     (-filter (-compose #'stringp #'car))
                     (-mapcat #'cdr)))
         (begin-re (->> pairs (-map #'car) (regexp-opt)))
         (end-re   (->> pairs (-map #'cdr) (regexp-opt))))
    (font-lock-add-keywords
     'web-mode
     `((,(regexp-quote "<%= ")
        ("\\<\\(case\\|cond\\)\\>"
         (save-match-data
           (save-excursion
             (setq-local font-lock--web-mode-anchor-start-pos (point))
             (re-search-forward ,(regexp-quote " %>") (point-max))
             (match-string 0)))
         (goto-char font-lock--web-mode-anchor-start-pos)
         (1 'web-mode-keyword-face))))
     :append)
    (font-lock-add-keywords
     'web-mode
     `((,begin-re
        ("\\([-_0-9A-Za-z]+\\)"
         (save-match-data
           (save-excursion
             (setq-local font-lock--web-mode-anchor-start-pos (point))
             (re-search-forward ,end-re (point-max))
             (match-string 0)))
         (goto-char font-lock--web-mode-anchor-start-pos)
         (1 'default)))
       ("."
        (0 'shadow)))
     :append)))

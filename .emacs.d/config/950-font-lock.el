;; lazy font-lock
(defvar font-lock-idle-time 0.05
  "TODO")

(defvar font-lock-idle-avoid-buf-regex (regexp-opt '("org-src-fontification"))
  "TODO")

(defvar font-lock-idle-avoid-cmds '(comment-dwim undo undo-tree-undo)
  "TODO")

(setq font-lock-idle-timer nil
      font-lock-idle-start nil
      font-lock-idle-end nil)
(make-local-variable 'font-lock-idle-start)
(make-local-variable 'font-lock-idle-end)
(make-local-variable 'font-lock-idle-timer)
(advice-add #'jit-lock-after-change :around
            (byte-compile
             (lambda (fn start end old-len)
               "Run lazily highlighting text."
               (if (or (not font-lock-idle-time)
                       (string-match-p font-lock-idle-avoid-buf-regex (buffer-name))
                       (-contains? font-lock-idle-avoid-cmds this-command))
                   (funcall fn start end old-len)
                 (when (timerp font-lock-idle-timer)
                   (let ((timer-fn (prog1 (timer--function font-lock-idle-timer)
                                     (cancel-timer font-lock-idle-timer)))
                         (start_ (or font-lock-idle-start start))
                         (end_   (or font-lock-idle-end   end)))
                     (when (= end_ start)  (setq start start_))
                     (when (= end  start_) (setq end end_))
                     (when (or (< end_   start)
                               (< end    start_)
                               (< start_ start  end_ end)
                               (< start  start_ end  end_))
                       (funcall timer-fn))))
                 (setq font-lock-idle-start start
                       font-lock-idle-end   end
                       font-lock-idle-timer
                       (run-with-idle-timer font-lock-idle-time nil
                                            (lexical-let ((args (list start end old-len))
                                                          (buf (current-buffer))
                                                          (fn fn))
                                              (lambda ()
                                                (setq font-lock-idle-timer nil)
                                                (ignore-errors
                                                  (with-current-buffer buf
                                                    (let (fancy-narrow--beginning fancy-narrow--end)
                                                      (apply fn args))))))))))))

(defvar fake-match-2 (-repeat 2 (point-min-marker))
  "TODO")

(defvar fake-match-4 (-repeat 4 (point-min-marker))
  "TODO")

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
    '((t (:inherit font-lock-comment-face)))
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

  (defcustom clojure-warning-if-pollute-core-namespace t
    "TODO"
    :type 'boolean
    :safe 'booleanp)

  (defconst clojure-core-regex
    (regexp-opt '("accessor"
                  "aclone"
                  "add-classpath"
                  "add-watch"
                  "agent"
                  "agent-error"
                  "agent-errors"
                  "aget"
                  "alength"
                  "alias"
                  "all-ns"
                  "alter"
                  "alter-meta!"
                  "alter-var-root"
                  "amap"
                  "ancestors"
                  "and"
                  "apply"
                  "areduce"
                  "array-map"
                  "aset"
                  "aset-boolean"
                  "aset-byte"
                  "aset-char"
                  "aset-double"
                  "aset-float"
                  "aset-int"
                  "aset-long"
                  "aset-short"
                  "assert"
                  "assoc"
                  "assoc!"
                  "assoc-in"
                  "associative?"
                  "atom"
                  "await"
                  "await-for"
                  "bases"
                  "bean"
                  "bigdec"
                  "bigint"
                  "biginteger"
                  "binding"
                  "bit-and"
                  "bit-and-not"
                  "bit-clear"
                  "bit-flip"
                  "bit-not"
                  "bit-or"
                  "bit-set"
                  "bit-shift-left"
                  "bit-shift-right"
                  "bit-test"
                  "bit-xor"
                  "boolean"
                  "boolean-array"
                  "booleans"
                  "bound-fn"
                  "bound-fn*"
                  "bound?"
                  "butlast"
                  "byte"
                  "byte-array"
                  "bytes"
                  "case"
                  "cast"
                  "cat"
                  "catch"
                  "char"
                  "char-array"
                  "char-escape-string"
                  "char-name-string"
                  "char?"
                  "chars"
                  "class"
                  "class?"
                  "clear-agent-errors"
                  "clojure-version"
                  "coll?"
                  "comment"
                  "commute"
                  "comp"
                  "comparator"
                  "compare"
                  "compare-and-set!"
                  "compile"
                  "complement"
                  "completing"
                  "concat"
                  "cond"
                  "condp"
                  "conj"
                  "conj!"
                  "cons"
                  "constantly"
                  "construct-proxy"
                  "contains?"
                  "count"
                  "counted?"
                  "create-ns"
                  "create-struct"
                  "cycle"
                  "dec"
                  "dec'"
                  "decimal?"
                  "declare"
                  "dedupe"
                  "def"
                  "default-data-readers"
                  "definline"
                  "definterface"
                  "defmacro"
                  "defmethod"
                  "defmulti"
                  "defn"
                  "defn-"
                  "defonce"
                  "defprotocol"
                  "defrecord"
                  "defstruct"
                  "deftype"
                  "delay"
                  "delay?"
                  "deliver"
                  "denominator"
                  "deref"
                  "derive"
                  "descendants"
                  "disj"
                  "disj!"
                  "dissoc"
                  "dissoc!"
                  "distinct"
                  "distinct?"
                  "do"
                  "doall"
                  "dorun"
                  "doseq"
                  "dosync"
                  "dotimes"
                  "doto"
                  "double"
                  "double-array"
                  "doubles"
                  "drop"
                  "drop-last"
                  "drop-while"
                  "eduction"
                  "empty"
                  "empty?"
                  "ensure"
                  "ensure-reduced"
                  "enumeration-seq"
                  "error-handler"
                  "error-mode"
                  "eval"
                  "even?"
                  "every-pred"
                  "every?"
                  "ex-data"
                  "ex-info"
                  "extend"
                  "extend-protocol"
                  "extend-type"
                  "extenders"
                  "extends?"
                  "false?"
                  "ffirst"
                  "file-seq"
                  "filter"
                  "filterv"
                  "finally"
                  "find"
                  "find-keyword"
                  "find-ns"
                  "find-var"
                  "first"
                  "flatten"
                  "float"
                  "float-array"
                  "float?"
                  "floats"
                  "flush"
                  "fn"
                  "fn?"
                  "fnext"
                  "fnil"
                  "for"
                  "force"
                  "format"
                  "frequencies"
                  "future"
                  "future-call"
                  "future-cancel"
                  "future-cancelled?"
                  "future-done?"
                  "future?"
                  "gen-class"
                  "gen-interface"
                  "gensym"
                  "get"
                  "get-in"
                  "get-method"
                  "get-proxy-class"
                  "get-thread-bindings"
                  "get-validator"
                  "group-by"
                  "hash"
                  "hash-map"
                  "hash-ordered-coll"
                  "hash-set"
                  "hash-unordered-coll"
                  "identical?"
                  "identity"
                  "if"
                  "if-let"
                  "if-not"
                  "if-some"
                  "ifn?"
                  "import"
                  "in-ns"
                  "inc"
                  "inc'"
                  "init-proxy"
                  "instance?"
                  "int"
                  "int-array"
                  "integer?"
                  "interleave"
                  "intern"
                  "interpose"
                  "into"
                  "into-array"
                  "ints"
                  "io!"
                  "isa?"
                  "iterate"
                  "iterator-seq"
                  "juxt"
                  "keep"
                  "keep-indexed"
                  "key"
                  "keys"
                  "keyword"
                  "keyword?"
                  "last"
                  "lazy-cat"
                  "lazy-seq"
                  "let"
                  "letfn"
                  "line-seq"
                  "list"
                  "list*"
                  "list?"
                  "load"
                  "load-file"
                  "load-reader"
                  "load-string"
                  "loaded-libs"
                  "locking"
                  "long"
                  "long-array"
                  "longs"
                  "loop"
                  "macroexpand"
                  "macroexpand-1"
                  "make-array"
                  "make-hierarchy"
                  "map"
                  "map-entry?"
                  "map-indexed"
                  "map?"
                  "mapcat"
                  "mapv"
                  "max"
                  "max-key"
                  "memfn"
                  "memoize"
                  "merge"
                  "merge-with"
                  "meta"
                  "methods"
                  "min"
                  "min-key"
                  "mix-collection-hash"
                  "mod"
                  "monitor-enter"
                  "monitor-exit"
                  "name"
                  "namespace"
                  "namespace-munge"
                  "neg?"
                  "new"
                  "newline"
                  "next"
                  "nfirst"
                  "nil?"
                  "nnext"
                  "not"
                  "not-any?"
                  "not-empty"
                  "not-every?"
                  "not="
                  "ns"
                  "ns-aliases"
                  "ns-imports"
                  "ns-interns"
                  "ns-map"
                  "ns-name"
                  "ns-publics"
                  "ns-refers"
                  "ns-resolve"
                  "ns-unalias"
                  "ns-unmap"
                  "nth"
                  "nthnext"
                  "nthrest"
                  "num"
                  "number?"
                  "numerator"
                  "object-array"
                  "odd?"
                  "or"
                  "parents"
                  "partial"
                  "partition"
                  "partition-all"
                  "partition-by"
                  "pcalls"
                  "peek"
                  "persistent!"
                  "pmap"
                  "pop"
                  "pop!"
                  "pop-thread-bindings"
                  "pos?"
                  "pr"
                  "pr-str"
                  "prefer-method"
                  "prefers"
                  "print"
                  "print-str"
                  "printf"
                  "println"
                  "println-str"
                  "prn"
                  "prn-str"
                  "promise"
                  "proxy"
                  "proxy-mappings"
                  "proxy-super"
                  "push-thread-bindings"
                  "pvalues"
                  "quot"
                  "quote"
                  "rand"
                  "rand-int"
                  "rand-nth"
                  "random-sample"
                  "range"
                  "ratio?"
                  "rational?"
                  "rationalize"
                  "re-find"
                  "re-groups"
                  "re-matcher"
                  "re-matches"
                  "re-pattern"
                  "re-seq"
                  "read"
                  "read-line"
                  "read-string"
                  "reader-conditional"
                  "reader-conditional?"
                  "realized?"
                  "record?"
                  "recur"
                  "reduce"
                  "reduce-kv"
                  "reduced"
                  "reduced?"
                  "reductions"
                  "ref"
                  "ref-history-count"
                  "ref-max-history"
                  "ref-min-history"
                  "ref-set"
                  "refer"
                  "refer-clojure"
                  "reify"
                  "release-pending-sends"
                  "rem"
                  "remove"
                  "remove-all-methods"
                  "remove-method"
                  "remove-ns"
                  "remove-watch"
                  "repeat"
                  "repeatedly"
                  "replace"
                  "replicate"
                  "require"
                  "reset!"
                  "reset-meta!"
                  "resolve"
                  "rest"
                  "restart-agent"
                  "resultset-seq"
                  "reverse"
                  "reversible?"
                  "rseq"
                  "rsubseq"
                  "run!"
                  "satisfies?"
                  "second"
                  "select-keys"
                  "send"
                  "send-off"
                  "send-via"
                  "seq"
                  "seq?"
                  "seque"
                  "sequence"
                  "sequential?"
                  "set"
                  "set!"
                  "set-agent-send-executor!"
                  "set-agent-send-off-executor!"
                  "set-error-handler!"
                  "set-error-mode!"
                  "set-validator!"
                  "set?"
                  "short"
                  "short-array"
                  "shorts"
                  "shuffle"
                  "shutdown-agents"
                  "slurp"
                  "some"
                  "some-fn"
                  "some?"
                  "sort"
                  "sort-by"
                  "sorted-map"
                  "sorted-map-by"
                  "sorted-set"
                  "sorted-set-by"
                  "sorted?"
                  "special-symbol?"
                  "spit"
                  "split-at"
                  "split-with"
                  "str"
                  "string?"
                  "struct"
                  "struct-map"
                  "subs"
                  "subseq"
                  "subvec"
                  "supers"
                  "swap!"
                  "symbol"
                  "symbol?"
                  "sync"
                  "tagged-literal"
                  "tagged-literal?"
                  "take"
                  "take-last"
                  "take-nth"
                  "take-while"
                  "test"
                  "the-ns"
                  "thread-bound?"
                  "throw"
                  "time"
                  "to-array"
                  "to-array-2d"
                  "trampoline"
                  "transduce"
                  "transient"
                  "tree-seq"
                  "true?"
                  "try"
                  "type"
                  "unchecked-add"
                  "unchecked-add-int"
                  "unchecked-byte"
                  "unchecked-char"
                  "unchecked-dec"
                  "unchecked-dec-int"
                  "unchecked-divide-int"
                  "unchecked-double"
                  "unchecked-float"
                  "unchecked-inc"
                  "unchecked-inc-int"
                  "unchecked-int"
                  "unchecked-long"
                  "unchecked-multiply"
                  "unchecked-multiply-int"
                  "unchecked-negate"
                  "unchecked-negate-int"
                  "unchecked-remainder-int"
                  "unchecked-short"
                  "unchecked-subtract"
                  "unchecked-subtract-int"
                  "underive"
                  "unreduced"
                  "unsigned-bit-shift-right"
                  "update"
                  "update-in"
                  "update-proxy"
                  "use"
                  "val"
                  "vals"
                  "var"
                  "var-get"
                  "var-set"
                  "var?"
                  "vary-meta"
                  "vec"
                  "vector"
                  "vector-of"
                  "vector?"
                  "volatile!"
                  "volatile?"
                  "vreset!"
                  "vswap!"
                  "when"
                  "when-first"
                  "when-let"
                  "when-not"
                  "when-some"
                  "while"
                  "with-bindings"
                  "with-bindings*"
                  "with-in-str"
                  "with-local-vars"
                  "with-meta"
                  "with-open"
                  "with-out-str"
                  "with-precision"
                  "with-redefs"
                  "with-redefs-fn"
                  "xml-seq"
                  "zero?"
                  "zipmap")
                'symbols)
    "TODO")

  (defcustom clojure--binding-forms
    '("binding" "doseq" "dotimes" "for" "let" "if-let" "if-some" "when-let" "when-some" "loop" "with-redefs")
    "List of Clojure binding form."
    :type '(repeat string)
    :safe (lambda (value)
            (and (listp value)
                 (cl-every 'stringp value))))

  (defun clojure--binding-regexp ()
    "TODO"
    (concat "(" (regexp-opt clojure--binding-forms) "[ \r\t\n]+\\["))

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

  (let* ((whitespace "[ \r\t\n]")
         (whitespace+ (concat whitespace "+"))
         (whitespace* (concat whitespace "*"))
         (symbol clojure--sym-regexp)
         (symbol? (concat "\\(?:" symbol "\\)?"))
         (namespace (concat "\\(?:" symbol "/\\)"))
         (namespace? (concat namespace "?"))
         (meta? "\\(?:\\(?:#\\^{[^}]*}\\|\\^:?\\sw+\\)[ \r\n\t]+\\)?")
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
            (byte-compile
             (lambda (meta?+symbol limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (re-search-forward meta?+symbol limit)
                 (clojure-skip :vector)
                 (set-match-data
                  (if (looking-at-p "\"")
                      (list (point-marker) (progn (forward-sexp) (point-marker)))
                    fake-match-2))
                 t)))
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
            (byte-compile
             (lambda (symbol+alter-mark limit)
               (when (re-search-forward symbol+alter-mark limit 'no-err)
                 (let ((face (plist-get (text-properties-at (match-beginning 1)) 'face))
                       (ignore-faces '(font-lock-doc-face
                                       font-lock-string-face
                                       font-lock-comment-face)))
                   (when (memq face ignore-faces)
                     (set-match-data fake-match-4))
                   t))))
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
         (,(concat "(" core-ns? "def[a-z]*" whitespace+ "\\^")
          (,(byte-compile
             (lambda (limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (re-search-forward "\\([-0-9A-Za-z:]+\\)" limit))))
           (save-excursion
             (if (in-comment?)
                 (setq font-lock--skip t)
               (setq font-lock--skip nil)
               (setq clojure-meta---point (point))
               (clojure-forward-sexp)
               (point)))
           (if font-lock--skip
               (end-of-line)
             (goto-char clojure-meta---point))
           (1 'clojure-meta-face)))
         ;; Binding forms
         (,(concat "(" core-ns? (substring (clojure--binding-regexp) 1))
          ;; Normal bindings
          (,(-partial
             (byte-compile
              (lambda (meta?+ns?+symbol limit)
                (ignore-errors
                  (when font-lock--skip
                    (error ""))
                  (clojure-skip :comment :ignored-form :type-hint :destructuring-bind)
                  (let ((local-limit (save-excursion (forward-sexp) (point))))
                    (unless (and (re-search-forward meta?+ns?+symbol (min local-limit limit) t)
                                 (not (string-match-p clojure--ignore-binding-highlight-regex
                                                      (match-string-no-properties 1))))
                      (set-match-data fake-match-4))
                    (goto-char local-limit))
                  (clojure-forward-sexp)
                  t)))
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
           (1 (if (and clojure-warning-if-pollute-core-namespace
                       (string-match-p clojure-core-regex (or (match-string 1) "")))
                  'clojure-local-binding-variable-name-warning-face
                'clojure-local-binding-variable-name-face)))
          ;; Destructuring bindings
          (,(-partial
             (byte-compile
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
                            (set-match-data fake-match-4))
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
                      (set-match-data fake-match-4))
                    t))))
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
           (1 (if (and clojure-warning-if-pollute-core-namespace
                       (string-match-p clojure-core-regex (or (match-string 1) "")))
                  'clojure-local-binding-variable-name-warning-face
                'clojure-local-binding-variable-name-face))))
         ;; OOP style function forms & letfn
         (,(concat "(" core-ns? "\\(" oop-kw whitespace+ "\\|" "letfn" whitespace+ "\\[" "\\)")
          ;; highlighting definition type symbol
          (,(-partial
             (byte-compile
              (lambda (symbol limit)
                (and (string-match-p "^def" clojure-oop-kw--str)
                     (re-search-forward symbol limit t))))
             (concat "\\(" symbol "\\)"))
           (save-excursion
             (setq clojure-oop-kw--str (match-string-no-properties 1))
             (setq clojure-oop-kw--point (point))
             (clojure-forward-sexp)
             (point))
           (goto-char clojure-oop-kw--point)
           (0 'clojure-define-type-face))
          ;; highlighting OOP fn name
          (,(-partial
             (byte-compile
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
                        (set-match-data fake-match-2)))
                    (up-list)
                    t))))
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
             (byte-compile
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
                                      clojure-oop-fn-form--points)))
                        )))
                  (when clojure-oop-fn-recursive--point
                    (if (re-search-forward meta?+ns?+symbol
                                           (min limit clojure-oop-fn-recursive--limit) t)
                        (when (string-match-p clojure--ignore-binding-highlight-regex
                                              (match-string-no-properties 1))
                          (set-match-data fake-match-4))
                      (set-match-data fake-match-4)
                      (setq clojure-oop-fn-recursive--point nil
                            clojure-oop-fn-recursive--limit nil))
                    t))))
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
           (1 (if (and clojure-warning-if-pollute-core-namespace
                       (string-match-p clojure-core-regex (or (match-string 1) "")))
                  'clojure-fn-parameter-warning-face
                'clojure-fn-parameter-face))))
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
             (byte-compile
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
                            (set-match-data fake-match-4))
                          ;; Handle default bind map
                          (when (save-excursion
                                  (backward-up-list)
                                  (and (char-equal ?{ (char-after))
                                       (ignore-errors
                                         (clojure-forward-sexp -1)
                                         (looking-at-p ":or\\>"))))
                            (clojure-forward-sexp)))
                      (set-match-data fake-match-4)
                      (setq clojure-fn-recursive--point nil
                            clojure-fn-recursive--limit nil))
                    t))))
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
           (1 (if (and clojure-warning-if-pollute-core-namespace
                       (string-match-p clojure-core-regex (or (match-string 1) "")))
                  'clojure-fn-parameter-warning-face
                'clojure-fn-parameter-face))))
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
         (,(concat "(" important-kw "\\(?:)\\|" whitespace "\\)")
          (1 'clojure-important-keywords-face))
         (,(concat "::\\(" symbol "\\)/" symbol "\\>")
          (1 'font-lock-type-face)))))))

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
                      (set-match-data fake-match-4))
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
         (,(concat "\\(defun\\|lambda\\)" whitespace+ "\\(" symbol whitespace+ "\\)?(")
          (,(byte-compile
             (-partial
              (lambda (symbol limit)
                (ignore-errors
                  (when font-lock--skip
                    (error ""))
                  (when (re-search-forward symbol limit t)
                    (when (string-match-p "^&" (match-string 1))
                      (set-match-data fake-match-4))
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
                (list :family "FontAwesome"
                      :foreground (face-attribute (if (string-equal x "X")
                                                      'org-done 'org-todo)
                                                  :foreground)))) t)
         ("\\(\\\\\\\\\\)\\s-*$"
          1 'shadow nil)))
     :append)))

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
     `((,(concat "\\(" symbol "\\|->[_0-9a-zA-Z]+\\)")
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

(use-package sh-script
  :defer t
  :config
  (font-lock-add-keywords
   'sh-mode
   (let* ((symbol "[@?_0-9a-zA-Z]+")
          (symbol_ (concat "\\(?:\\$" symbol "\\|\\${" symbol "}\\)"))
          (whitespace "[ \r\t]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*"))
          (assigment "=[^=]"))
     `((,(concat "\\(" symbol_ "\\)")
        (1 (let ((face (plist-get (text-properties-at (1- (match-beginning 0))) 'face)) face-lst)
             (setq face-lst (if (listp face) face (list face)))
             (when (or (memq 'font-lock-comment-face face-lst)
                       (memq 'font-lock-string-face  face-lst))
               face))
           t))))
   :append))

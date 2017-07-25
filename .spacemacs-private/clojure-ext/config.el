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
              'symbols))

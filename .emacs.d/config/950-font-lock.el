;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(setq font-lock--anchor-beg-point nil
      font-lock--local-limit nil)
(make-local-variable 'font-lock--local-limit)
(make-local-variable 'font-lock--anchor-beg-point)

(defun fake-match-2 ()
  "TODO"
  (-repeat 2 (point-min-marker)))

(defun fake-match-4 ()
  "TODO"
  (-repeat 4 (point-min-marker)))

(let ((f (lambda ()
           (font-lock-add-keywords
            nil
            '(("\\([,]\\|[|&]\\{2,2\\}\\|\\s(\\|\\s)\\)"
               (1 'shadow)))
            :append))))
  (add-hook 'git-timemachine-mode-hook f :append)
  (add-hook 'toml-mode-hook f :append)
  (add-hook 'prog-mode-hook
            (lambda ()
              (cond
               ((derived-mode-p 'clojure-mode))
               (t (funcall f))))
            :append))

(defface symbol-dash-or-underline-face
  '((t (:inherit shadow)))
  "TODO")


(use-package cc-mode
  :defer t
  :config
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (when (member major-mode '(c-mode c++-mode))
       (font-lock-add-keywords
        nil
        `(;; punctuation
          ("\\([-=]>\\|::\\|[.;]\\)"
           (1 'shadow))
          ("\\(\\\\\\)$"
           (1 'shadow))
          ("[A-Za-z]\\(:\\)\\s-*$"
           (1 'shadow))
          ("[_A-Za-z]\\(\\*+\\|&\\)\\(?:\\s-\\|\\s(\\)"
           (1 'shadow))
          ("\\(?:\\s-\\|\\s(\\)\\(\\*+\\|&\\)[_A-Za-z()]"
           (1 'shadow)))
        :append))
     (font-lock-add-keywords
      nil
      '(;; punctuation
        ("\\([.;]\\)"
         (1 'shadow))
        ("[A-Za-z]\\(:\\)\\s-*$"
         (1 'shadow))
        ("[0-9A-Za-z]\\(_\\)[0-9A-Za-z]"
         (1 'symbol-dash-or-underline-face prepend)))
      :append))))

(use-package elixir-mode
  :disabled t
  :defer t
  :init
  (defface elixir-argument-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "TODO")

  :config
  (let* ((symbol "[_0-9a-zA-Z?!]+")
         (symbol-fn (lambda (limit)
                      (let ((found-p nil))
                        (while (and (re-search-forward (concat "\\_<\\(" symbol "\\)\\_>") limit t)
                                    (not found-p))
                          (unless (equal ?^ (char-before (match-beginning 1)))
                            (setq found-p t)))
                        found-p))))
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
  (let ((kws '("and" "bless" "defined" "delete" "exists" "grep" "keys" "join" "lc" "map" "not" "or" "push" "ref" "splice" "xor")))
    (font-lock-add-keywords
     'cperl-mode
     `((,(regexp-opt kws 'symbols)
        1 'font-lock-keyword-face))))
  (font-lock-add-keywords
   'cperl-mode
   (let* ((symbol "[@$%]+[:_0-9a-zA-Z]+")
          (whitespace "[ \r\t\n]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*")))
     `((,(concat "\\(" symbol "\\|\\(accept\\|do\\)\\s-*(\\)")
        (1 (cond
            ((in-string?)  'font-lock-string-face)
            ((in-comment?) 'font-lock-comment-face)
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
       ("\\([\\&|*]\\|::\\|;\\|[-=]>\\|[$@]_\\>\\)"
        (1 'shadow))
       ("\\([*@$%]+\\)\\(?:[:_0-9a-zA-Z]\\|\\s(\\)"
        (1 'shadow))
       ("[0-9A-Za-z]\\(_\\)[0-9A-Za-z]"
        (1 'symbol-dash-or-underline-face prepend))))
   :append))

(use-package clojure-mode
  :defer t
  :init
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
  (defface clojure-special-variable-definition-face
    `((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Clojure special variable name.")
  (defface clojure-local-binding-variable-name-face
    `((t (:inherit font-lock-variable-name-face :weight ,(face-attribute 'default :weight))))
    "Face used to font-lock Clojure local binding variable name.")
  (defface clojure-local-binding-variable-name-unsed-face
    '((t (:inherit shadow)))
    "TODO")
  (defface clojure-fn-parameter-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Clojure parameter.")
  (defface clojure-fn-parameter-unused-face
    '((t (:inherit shadow)))
    "TODO")
  (defface clojure-semi-function-name-face
    `((t (:inherit font-lock-function-name-face :weight ,(face-attribute 'default :weight))))
    "Face used to font-lock Clojure OOP style functions.")
  (defface clojure-cond-condtion-face
    '((t (:inherit italic)))
    "Face used to font-lock Clojure conditions in `cond' form.")
  (defface clojure-define-type-face
    '((t (:inherit (font-lock-type-face))))
    "TODO")
  (defface clojure-meta-face
    '((t (:inherit shadow)))
    "TODO")
  (defface clojure-interop-method-face
    '((t (:inherit font-lock-keyword-face)))
    "TODO")
  (defface clojure-variable-definition-face
    '((t (:inherit font-lock-variable-name-face)))
    "TODO")
  (defface clojure-comment-sexp-face
    '((t (:inherit shadow)))
    "TODO")
  (defface clojure-punctuation-face
    '((t (:inherit shadow)))
    "TODO")

  (defcustom clojure--ignore-binding-highlight-regex
    "^\\([&_]\\)$"
    "TODO"
    :type '(repeat string)
    :safe (lambda (value)
            (and (listp value)
                 (cl-every 'stringp value))))

  (defcustom clojure-spc-key-valign-skip 3
    "TODO"
    :type 'integer
    :safe 'integerp)

  :config
  (setq-local clojure-cond-newline--limit nil)
  (setq-local clojure-binding-form--recursive-point nil)
  (setq-local clojure-binding-form--recursive-limit nil)
  (setq-local clojure-oop-kw--str nil)
  (setq-local clojure-oop-fn-form--points nil)
  (setq-local clojure-oop-fn-recursive--point nil)
  (setq-local clojure-oop-fn-recursive--limit nil)
  (setq-local clojure-fn-form--method? nil)
  (setq-local clojure-fn-form--multi-arity? nil)
  (setq-local clojure-fn-recursive--point nil)
  (setq-local clojure-fn-recursive--limit nil)

  (defconst clojure-font-lock-keywords
    (let* ((whitespace "[ \r\t\n]")
           (whitespace+ (concat whitespace "+"))
           (whitespace* (concat whitespace "*"))
           (symbol  clojure--sym-regexp)
           (symbol? (concat "\\(?:" symbol "\\)?"))
           (namespace  (concat "\\(?:" symbol "/\\)"))
           (namespace? (concat namespace "?"))
           (meta? "\\(?:\\(?:#?\\^{[^^]+}\\|\\^:?\\sw+\\)[ \r\n\t]+\\)?")
           (core-ns  (concat (regexp-opt '("clojure.core" "cljs.core" "core") nil) "/"))
           (core-ns? (concat "\\(?:" core-ns "\\)?"))
           (oop-kw  (regexp-opt '("definterface" "defprotocol" "defrecord" "deftype" "extend-protocol" "extend-type" "proxy" "reify")))
           (def-kw  (regexp-opt '("defmacro" "defn" "defn-" "defmethod" "defrecord" "deftype") t))
           (highlight-kw (regexp-opt '("go-loop" "with-hard-redefs" "proxy" "reify") t))
           (clojure--binding-kw
            '("binding" "doseq" "dotimes" "for" "let" "if-let" "if-some" "when-let" "when-some" "loop" "go-loop" "with-redefs")))

      `(;; Binding forms
        (,(concat "(" namespace? (regexp-opt clojure--binding-kw) "[ \r\t\n]+\\[")
         ;; Normal bindings
         (,(let ((meta?+ns?+symbol (concat meta? "\\_<" namespace? "\\(" symbol "\\)#?\\>")))
             (lambda (limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (clojure-skip :comment :ignored-form :metadata :destructuring-bind)
                 (let ((local-limit (save-excursion (forward-sexp) (point))))
                   (unless (and (re-search-forward meta?+ns?+symbol (min local-limit limit) t)
                                (not (string-match-p clojure--ignore-binding-highlight-regex
                                                     (match-string-no-properties 1))))
                     (set-match-data (fake-match-4)))
                   (goto-char local-limit))
                 (clojure-forward-sexp)
                 t)))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--anchor-beg-point (point)
                    font-lock--skip nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                 'clojure-local-binding-variable-name-unsed-face
               'clojure-local-binding-variable-name-face)))

         ;; Destructuring bindings
         (,(let ((meta?+ns?+symbol (concat meta? "\\_<" namespace? "\\(" symbol "\\)#?\\>")))
             (lambda (limit)
               ;; NOTE
               ;; We need to iterate to search symbols in the destructuring form,
               ;; but anchored-matcher does not support recursion.
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (unless clojure-binding-form--recursive-point
                   (while (and (> limit (point))
                               (prog1 t (clojure-skip :comment :ignored-form :metadata))
                               ;; skip normal bind?
                               (not (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)"))
                               (prog1 t (clojure-forward-sexp 2))))
                   (when (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)")
                     (setq clojure-binding-form--recursive-point (progn (down-list) (point))
                           clojure-binding-form--recursive-limit (save-excursion (up-list) (point)))))
                 (when clojure-binding-form--recursive-point
                   (clojure-skip :comment :ignored-form :metadata)
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
                   t))))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--anchor-beg-point (point)
                    font-lock--skip nil
                    clojure-binding-form--recursive-point nil
                    clojure-binding-form--recursive-limit nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                 'clojure-local-binding-variable-name-unsed-face
               'clojure-local-binding-variable-name-face))))

        ;; OOP style function forms & letfn
        (,(concat "(" core-ns? "\\(" oop-kw whitespace+ meta? "\\|" "letfn" whitespace+ "\\[" "\\)")
         ;; highlighting definition type symbol
         (,(let ((symbol (concat "\\(" symbol "\\)")))
             (lambda (limit)
               (unless font-lock--skip
                 (and (string-match-p "^def" clojure-oop-kw--str)
                      (re-search-forward symbol limit t)))))
          (save-excursion
            (setq clojure-oop-kw--str (match-string-no-properties 1)
                  font-lock--anchor-beg-point (point)
                  font-lock--skip nil)
            (condition-case nil
                (clojure-forward-sexp)
              (error (setq font-lock--skip t)))
            (point))
          (goto-char font-lock--anchor-beg-point)
          (0 'clojure-define-type-face))
         ;; highlighting OOP fn name
         (,(let ((symbol (concat symbol "\\>")))
             (lambda (limit)
               (ignore-errors
                 (when font-lock--skip
                   (error ""))
                 (while (and (> limit (point))
                             (prog1 t (clojure-skip :comment :ignored-form :metadata))
                             (not (looking-at-p "[ \r\t\n]*("))
                             (prog1 t (forward-sexp))))
                 (when (looking-at-p "[ \r\t\n]*(")
                   (down-list)
                   (clojure-skip :metadata :ignored-form)
                   (let ((local-limit (save-excursion (forward-sexp) (point))))
                     (if (re-search-forward symbol (min limit local-limit) t)
                         (add-to-list 'clojure-oop-fn-form--points (match-end 0))
                       (set-match-data (fake-match-2))))
                   (up-list)
                   t))))
          (let ((beg (match-beginning 0)))
            (setq clojure-oop-fn-form--points nil)
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-semi-function-name-face))

         ;; Highlighting OOP fn parameters
         (,(let ((meta?+ns?+symbol (concat meta? "\\_<" namespace? "\\(" symbol "\\)#?\\>")))
             (lambda (limit)
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
                   t))))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (setq clojure-oop-fn-recursive--point nil)
              (setq clojure-oop-fn-recursive--limit nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                 'clojure-fn-parameter-unused-face
               'clojure-fn-parameter-face))))

        ;; Clojure definition keywords - (DEFINITION-KEYWORD symbol ...)
        (,(concat "(" core-ns? def-kw "\\>" whitespace+ meta? "\\(" symbol "*?!*\\)\\>")
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face)

         ;; fn parameters highlight
         (,(let ((meta?+ns?+symbol (concat meta? "\\_<" namespace? "\\(" symbol "\\)#?\\>")))
             (lambda (limit)
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
                   t))))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (setq clojure-fn-form--method? (string-match-p "defmethod" (match-string 1)))
              (setq clojure-fn-form--multi-arity? nil)
              (setq clojure-fn-recursive--point nil)
              (setq clojure-fn-recursive--limit nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                 'clojure-fn-parameter-unused-face
               'clojure-fn-parameter-face))))

        ;; Clojure type/protocol extention keyword
        (,(concat "(" core-ns? "\\(" (regexp-opt '("extend" "extend-type" "extend-protocol")) "\\)\\>"
                  whitespace+ symbol)
         (1 'font-lock-keyword-face)
         ;; Highlighting type or protocol
         (,(lambda (limit)
             (unless font-lock--skip
               (when (re-search-forward symbol limit t)
                 (forward-sexp 1)
                 t)))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--anchor-beg-point (point)
                    font-lock--skip nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'font-lock-type-face)))

        ;; Highlighting for (:require ...)
        (,(concat "(:require" whitespace+)
         (,(let ((req-block (concat "\\[\\(" symbol "\\)\\(?:" whitespace+
                                    ":as" whitespace+ "\\(" symbol "\\)\\)?\\(?:" whitespace+
                                    ":refer" whitespace+ "\\[.+?\\]" whitespace* "\\)?\\]")))
             (lambda (limit)
               (unless font-lock--skip
                 (when (re-search-forward req-block limit t)
                   (let ((match-data (match-data)))
                     (set-match-data (append match-data (fake-match-2))))
                   (unless (match-data 2)
                     (set-match-data (list (match-data 0)
                                           (match-data 1)
                                           (match-data 1))))
                   t))))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--anchor-beg-point (point)
                    font-lock--skip nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 'font-lock-type-face)
          (2 'font-lock-type-face)))

        ;; %
        ("\\<%[&1-9]?\\>"
         (0 'clojure-special-variable-name-face))

        ;; definition dynamic variables
        (,(concat "(" core-ns? "def[a-z]*\\s-+" meta? "\\(\\*" symbol "\\*\\)\\_>")
         (1 'clojure-special-variable-definition-face))

        (,(concat "\\_<@?" namespace? "\\(\\*" symbol "\\*\\)\\_>")
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

        ;; Namespaced keyword - ::namespace/keyword
        (,(concat "::\\(" symbol "\\)\\(/\\)" symbol "\\>")
         (1 'font-lock-type-face)
         (2 'shadow))

        ;; Auto-gensym variable - variable#
        (,(concat symbol "\\(#\\)\\_>")
         (1 'clojure-punctuation-face))

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
        (,(concat "(" core-ns? "\\(defstruct\\)\\>"
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
         (2 'clojure-variable-definition-face nil t))

        ;; (fn name? args ...)
        (,(concat "(" core-ns? "\\(fn\\)"
                  whitespace+
                  meta?
                  "\\(" symbol "\\)?" )
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face nil t)

         ;; fn parameters highlight
         (,(let ((meta?+ns?+symbol (concat meta? "\\_<" namespace? "\\(" symbol "\\)#?\\>")))
             (lambda (limit)
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
                   t))))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (setq clojure-fn-form--multi-arity? nil)
              (setq clojure-fn-recursive--point nil)
              (setq clojure-fn-recursive--limit nil)
              (save-excursion
                (safe-up-list-1)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                 'clojure-fn-parameter-unused-face
               'clojure-fn-parameter-face))))

        ;; (default ....) or (namespace/default ...)
        (,(concat "(" namespace? "\\(default\\)\\>")
         (1 nil))

        ;; definition with keyword - (def :keyword ...) (def ::keyword ...) or (def :namespace/keyword ...)
        (,(concat "(" namespace? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                  whitespace+
                  meta?
                  "\\(:\\)\\(" symbol "\\)/\\(" symbol "\\)\\>")
         (1 'font-lock-keyword-face)
         (2 'clojure-defining-spec-face)
         (3 'font-lock-type-face)
         (4 'clojure-defining-spec-face))
        (,(concat "(" namespace? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                  whitespace+
                  meta?
                  "\\(::?\\)\\(" symbol "\\)\\>")
         (1 'font-lock-keyword-face)
         (2 'clojure-defining-spec-face)
         (3 'clojure-defining-spec-face))

        ;; Special forms
        (,(concat
           "(" (regexp-opt '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
                             "recur" "try" "finally" "recur" "throw" "catch"
                             "set!" "new" "."
                             "monitor-enter" "monitor-exit" "quote") t)
           "\\>")
         (1 'font-lock-keyword-face))

        ;; Built-in binding and flow of control forms
        (,(concat
           "(" core-ns?
           (regexp-opt '("letfn" "case" "cond" "cond->" "cond->>" "condp"
                         "for" "when" "when-not" "when-let" "when-first" "when-some"
                         "if-let" "if-not" "if-some"
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
        ("\\\\\\([[:punct:]]\\|\\s(\\|\\s)\\|[0-9A-Za-z]\\>\\|u[0-9A-Fa-f]+\\|newline\\)"
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
         (1 'clojure-comment-sexp-face t))
        (,(concat "(" core-ns? "comment\\_>")
         (,(lambda (limit)
             (when (< (point) limit)
               (let ((beg (point-marker))
                     (end (point-marker)))
                 (set-marker beg (point))
                 (set-marker end (1- limit))
                 (set-match-data (list beg end))
                 (goto-char limit))
               t))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--skip nil)
              (setq font-lock--anchor-beg-point (point))
              (save-excursion
                (safe-up-list-1)
                (point))))
          (when font-lock--skip
            (end-of-line))
          (0 'clojure-comment-sexp-face t)))

        ;; Highlight `code` marks, just like `elisp'.
        (,(rx "`" (group-n 1 (optional "#'") (+ (or (syntax symbol) (syntax word)))) "`")
         (1 'font-lock-constant-face prepend))

        ;; Highlight escaped characters in strings.
        (clojure-font-lock-escaped-chars
         (0 'bold prepend))

        ;; Highlight grouping constructs in regular expressions
        (clojure-font-lock-regexp-groups
         (1 'font-lock-regexp-grouping-construct prepend))

        ;; Metadata
        (,(concat "\\(?:" whitespace "\\|[([{]\\)\\^[:A-Za-z{]")
         (,(lambda (limit)
             (unless font-lock--skip
               (let ((start (progn
                              (backward-char 1)
                              (point-marker))))
                 (goto-char limit)
                 (set-match-data (list start (point-marker)))
                 t)))
          (let ((beg (match-beginning 0)))
            (if (in-comment-or-string? beg)
                (prog1 beg
                  (setq font-lock--skip t))
              (setq font-lock--anchor-beg-point (point)
                    font-lock--skip nil)
              (backward-char 1)
              (save-excursion
                (clojure-forward-sexp)
                (point))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-meta-face t)))

        ;; side-effect
        (,(concat symbol "?\\(!+\\)\\>")
         (1 (unless (-intersection '(font-lock-comment-face
                                     font-lock-doc-face
                                     font-lock-doc-string-face
                                     font-lock-string-face)
                                   (-list (get-text-property (match-beginning 1) 'face)))
              'clojure-side-effect-face)
            append))

        ;; Interop type name
        (,(concat "\\(" symbol "\\(\\." symbol "\\)+\\)")
         (1 'font-lock-type-face nil))
        (,(concat "\\<\\(\\([A-Z]" symbol "\\)\\)\\>")
         (1 'font-lock-type-face nil))

        ;; Custom keywords
        (,(concat "(" namespace? highlight-kw "\\_>")
         (1 'font-lock-keyword-face))

        ;; Highlight condtions in `case' and `cond', `condp', `cond->', `cond->>' form.
        (,(concat "(" core-ns? "\\(\\(?:case\\|cond\\(?:p\\|->>?\\)?\\)\\)[ \r\t\n]+")
         (,(lambda (limit)
             (when (and (not font-lock--skip)
                        (< (point) limit))
               (ignore-errors
                 (let ((beg (progn
                              (if (null clojure-cond-newline--limit)
                                  (clojure-skip :comment :ignored-form)
                                (forward-line)
                                (beginning-of-line-text))
                              (point-marker)))
                       (end (let ((line-end (line-end-position))
                                  (sexp-end (or clojure-cond-newline--limit
                                                (progn (clojure-forward-sexp) (point))))
                                  (m (make-marker)))
                              (if (<= sexp-end line-end)
                                  (progn
                                    (set-marker m (goto-char sexp-end))
                                    (setq clojure-cond-newline--limit nil)
                                    (clojure-forward-sexp))
                                (clojure-forward-sexp) ; check default case
                                (set-marker m (goto-char line-end))
                                (setq clojure-cond-newline--limit sexp-end))
                              m)))
                   (set-match-data (list beg end)))
                 t)))
          (prog1 (let ((beg (match-beginning 0)))
                   (if (in-comment-or-string? beg)
                       (prog1 beg
                         (setq font-lock--skip t))
                     (setq font-lock--anchor-beg-point (point)
                           font-lock--skip nil
                           clojure-cond-newline--limit nil)
                     (save-excursion
                       (safe-up-list-1)
                       (point))))
            (cond
             ((or (string= "case" (match-string 1))
                  (string-match-p "->>?$" (match-string 1)))
              (condition-case nil
                  (clojure-forward-sexp)
                (error (setq font-lock--skip t))))
             ((or (string= "casep" (match-string 1))
                  (string= "condp" (match-string 1)))
              (condition-case nil
                  (clojure-forward-sexp 2)
                (error (setq font-lock--skip t))))))
          (if font-lock--skip
              (end-of-line)
            (goto-char font-lock--anchor-beg-point))
          (0 'clojure-cond-condtion-face prepend)))

        ;; Highlight exception variable
        (,(concat "(catch" whitespace+ "[0-9A-Za-z]+" whitespace+ "\\(" symbol "\\)#?")
         (1 (if (s-starts-with? "_" (match-string-no-properties 1))
                'clojure-local-binding-variable-name-unsed-face
              'clojure-local-binding-variable-name-face)))

        ;; Improve docstring
        (,(concat "(defprotocol" whitespace+ symbol "\\>")
         ,(let ((meta?+symbol (concat "(" meta? symbol "\\>")))
            (lambda (limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (re-search-forward meta?+symbol limit)
                (clojure-skip :vector)
                (set-match-data
                 (if (looking-at-p "\"")
                     (list (point-marker) (progn (forward-sexp) (point-marker)))
                   (fake-match-2)))
                t)))
         (let ((beg (match-beginning 0)))
           (if (in-comment-or-string? beg)
               (prog1 beg
                 (setq font-lock--skip t))
             (setq font-lock--skip nil)
             (setq font-lock--anchor-beg-point (point))
             (save-excursion
               (safe-up-list-1)
               (point))))
         (if font-lock--skip
             (end-of-line)
           (goto-char font-lock--anchor-beg-point))
         (0 'font-lock-doc-face t))))
    "Default expressions to highlight in Clojure mode.")

  (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
    (font-lock-add-keywords
     mode
     `(;; Punctuation
       ("\\([~#@&_,`'^]\\|\\s(\\|\\s)\\)"
        (1 'clojure-punctuation-face append))
       ("[A-Za-z]\\(-+>?\\|[._$]\\)[A-Za-z]"
        (1 'symbol-dash-or-underline-face prepend)))
     :append)))

(use-package elisp-mode
  :defer t
  :init
  (defface lisp-local-binding-variable-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Lisp local binding variable name.")

  :config
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (let* ((symbol "[-+*/=>$&?:_0-9a-zA-Z]+")
           (whitespace "[ \r\t\n]")
           (whitespace+ (concat whitespace "+"))
           (whitespace* (concat whitespace "*")))
      (font-lock-add-keywords
       mode
       `(("\\_<\\(\\?\\(?:\\\\\\)?[^ \t\r\n]\\)"
          (1 'font-lock-string-face))
         ("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
          (1 'default nil))
         ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
          (1 'font-lock-constant-face))
         ("(\\(assert\\)"
          (1 'font-lock-variable-name-face))
         (,(concat "(defun" whitespace+ "\\(" symbol "\\)")
          (1 'font-lock-function-name-face))
         ;; punctuation
         ("#?'\\|`\\|\\_<_\\_>\\|,@"
          (0 'shadow))))
      (font-lock-add-keywords
       mode
       `(;; local variables
         (,(concat "(\\(lexical-\\|-?when-\\|-?if-\\)?let\\*?" whitespace+ "(")
          (,(let ((symbol+whitespace (concat "(\\(" symbol "\\)" whitespace+)))
              (lambda (limit)
                (ignore-errors
                  (when font-lock--skip
                    (error ""))
                  (comment-forward (point-max))
                  (let ((local-limit (save-excursion (forward-sexp) (point))))
                    (unless (and (re-search-forward symbol+whitespace (min local-limit limit) t)
                                 (ignore-errors (forward-sexp) t))
                      (set-match-data (fake-match-4)))
                    (goto-char local-limit))
                  t)))
           (let ((beg (match-beginning 0)))
             (if (in-comment-or-string? beg)
                 (prog1 beg
                   (setq font-lock--skip t))
               (setq font-lock--skip nil
                     font-lock--anchor-beg-point (point))
               (save-excursion
                 (safe-up-list-1)
                 (point))))
           (if font-lock--skip
               (end-of-line)
             (goto-char font-lock--anchor-beg-point))
           (1 'lisp-local-binding-variable-name-face)))
         ;; function arguments
         (,(concat "\\_<\\(defun\\|lambda\\)" whitespace+ "\\(" symbol whitespace+ "\\)?(")
          (,(let ((symbol (concat "\\(" symbol "\\)\\>")))
              (lambda (limit)
                (unless font-lock--skip
                  (when (re-search-forward symbol limit t)
                    (when (string-match-p "^&" (match-string 1))
                      (set-match-data (fake-match-4)))
                    t))))
           (let ((beg (match-beginning 0)))
             (if (in-comment-or-string? beg)
                 (prog1 beg
                   (setq font-lock--skip t))
               (setq font-lock--skip nil)
               (setq font-lock--anchor-beg-point (point))
               (save-excursion
                 (safe-up-list-1)
                 (point))))
           (if font-lock--skip
               (end-of-line)
             (goto-char font-lock--anchor-beg-point))
           (1 'lisp-local-binding-variable-name-face)))
         ;; punctuation
         ("[a-z]\\(-+>?\\)[a-z]"
          (1 'symbol-dash-or-underline-face prepend)))
       :append))))

(use-package go-mode
  :defer t
  :init
  (defface golang-type-definition-face
    '((t (:inherit font-lock-type-face)))
    "TODO")
  (defface golang-interface-method-face
    '((t (:inherit font-lock-function-name-face)))
    "TODO")

  :config
  (font-lock-add-keywords
   'go-mode
   `(;; unsed variable
     ("\\<\\(_\\(?:[0-9A-Za-z]+\\)?\\)\\>\\s-*\\(?:,\\|:?=\\)"
      (1 'shadow t))
     ;; pointer
     ("\\(?:^\\|[  \t]\\|\\s(\\|\\s)\\)\\([*&]\\)[0-9A-Za-z]"
      (1 'shadow t))
     ;; slice
     ("\\[.*?\\(:\\).*?\\]"
      (1 'shadow t))
     ;; channel direction notion
     ("\\_<\\(chan<-\\)"
      (1 'font-lock-keyword-face t))
     ("\\(<-chan\\)\\_>"
      (1 'font-lock-keyword-face t))
     ;; punctuation
     ("\\(:\\)\\(\\s-+\\|\\s-*$\\)"
      (1 'shadow t))
     ("\\([;]\\)"
      (1 'shadow t))))
  (font-lock-add-keywords
   'go-mode
   `(;; interface
     ("^\\s-*type\\s-+\\([_0-9A-Za-z]+\\)\\s-+interface\\s-*{\\s-*$"
      (1 'golang-type-definition-face t)
      (,(lambda (limit)
          (unless font-lock--skip
            (re-search-forward "^\\s-*\\([_0-9A-Za-z]+\\)" limit t)))
       (save-excursion
         (setq font-lock--skip nil
               font-lock--anchor-beg-point (match-beginning 0))
         (safe-up-list-1)
         (point))
       (goto-char font-lock--anchor-beg-point)
       (1 'golang-interface-method-face)))
     ;; struct
     ("^\\s-*type\\s-+\\([_0-9A-Za-z]+\\)\\s-+struct\\s-*{\\s-*$"
      (1 'golang-type-definition-face t)
      (,(lambda (limit)
          (unless font-lock--skip
            (re-search-forward "^\\s-*\\([_0-9A-Za-z]+\\)\\s-+.+$" limit t)))
       (save-excursion
         (setq font-lock--skip nil
               font-lock--anchor-beg-point (match-beginning 0))
         (safe-up-list-1)
         (point))
       (goto-char font-lock--anchor-beg-point)
       (1 'font-lock-variable-name-face)))
     ;; embedded field
     ("^\\s-*type\\s-+\\([_0-9A-Za-z]+\\)\\s-+struct\\s-*{\\s-*$"
      (1 'golang-type-definition-face t)
      (,(lambda (limit)
          (unless font-lock--skip
            (re-search-forward "^\\s-*\\([_0-9A-Za-z]+\\)\\s-*$" limit t)))
       (save-excursion
         (setq font-lock--skip nil
               font-lock--anchor-beg-point (match-beginning 0))
         (safe-up-list-1)
         (point))
       (goto-char font-lock--anchor-beg-point)
       (1 'font-lock-type-face))))
   :append))

(use-package js
  :defer t
  :config
  (font-lock-add-keywords
   'js-mode
   '(("\\(?:async\\|export\\)\\s-+function\\s-+\\([_0-9A-Za-z]+\\)\\>"
      (1 'font-lock-function-name-face))))
  (font-lock-add-keywords
   'js-mode
   '(;; punctuation
     ("\\(:\\|[-=]>\\|;\\)"
      (1 'shadow)))
   :append))

(use-package json-mode
  :defer t
  :config
  (font-lock-add-keywords
   'json-mode
   '(;; punctuation
     ("\\(\"\\)[0-9]+\\(\"\\)"
      (1 'shadow)
      (2 'shadow))
     ("\\(:\\)"
      (1 'shadow)))
   :append))

(use-package org
  :defer t
  :init
  (defface org-list-face
    '((t (:inherit shadow)))
    "Org listing face")

  (defface org-parenthesis-context-face
    '((t (:inherit default)))
    "Org parenthesis face")

  (defface org-task-done
    '((t (:inherit org-headline-done)))
    "Org complete task face")

  :config
  (font-lock-add-keywords
   'org-mode
   ;; NOTE
   ;;  - Install 'FontAwesome' font
   ;;  - Find a character at https://github.com/domtronn/all-the-icons.el/blob/master/data/data-faicons.el
   (let* ((square       (string-to-char "\xf0c8"))
          (minus-square (string-to-char "\xf146"))
          (check-square (string-to-char "\xf14a"))
          (org-done-face (color-from 'org-done :foreground))
          (org-todo-face (color-from 'org-todo :foreground)))
     `(("^\\s-*\\(?:-\\|[0-9]+\\.\\)\\s-\\(\\[\\( \\|-\\|X\\)\\]\\)\\s-"
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
              (put-text-property s e 'display '(raise -0.1))
              (list :family "Font Awesome 5 Free"
                    :foreground (if (string-equal x "X")
                                    ,org-done-face
                                  ,org-todo-face))))
        t)
       ("^\\s-*\\(-\\) "
        1 'org-list-face)
       ("^\\s-*\\(\\([0-9][).]\\)\\) "
        1 'org-list-face)
       ("^\\s-*\\(?:[-+]\\|[0-9]+[).]\\)\\s-+\\[\\(?:X\\)\\]\\s-+\\([^\r\n]+\\)"
        1 'org-task-done prepend)
       ("\\((.+?)\\)"
        1 'org-parenthesis-context-face)
       ("\\(\\\\\\\\\\|:\\)\\s-*$"
        1 'shadow nil)))
   :append))

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
         (1 'shadow))
        ("\\(;\\|:$\\)"
         (1 'shadow))
        ("\\(&\\)\\$"
         (1 'shadow)))))
  (setq php-font-lock-keywords php-font-lock-keywords-3)
  (font-lock-add-keywords
   'php-mode
   '(("\\([$]\\)"
      (1 'shadow append))
     ("[0-9A-Za-z]\\(_\\)[0-9A-Za-z]"
      (1 'symbol-dash-or-underline-face prepend)))
   :append))

(use-package python
  :defer t
  :config
  (let ((regex `(,(rx (or line-start whitespace (syntax open-parenthesis) "," "=")
                      (group
                       (or
                        "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
                        "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
                        "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
                        "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
                        "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
                        "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
                        "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
                        "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
                        ;; Python 2:
                        "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
                        "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
                        "intern"
                        ;; Python 3:
                        "ascii" "breakpoint" "bytearray" "bytes" "exec"))
                      "(")
                 . (1 font-lock-builtin-face))))
    (if (version<= "28.0.50" emacs-version)
        (progn
          (setf (cadddr python-font-lock-keywords-level-2)                  regex
                (car (cddddr python-font-lock-keywords-maximum-decoration)) regex)
          (setq python-font-lock-keywords-maximum-decoration
                (butlast python-font-lock-keywords-maximum-decoration 3))) ; remove assigment highlightings
      (setf (cadddr python-font-lock-keywords-level-2)            regex
            (cadddr python-font-lock-keywords-maximum-decoration) regex)
      (setq python-font-lock-keywords-maximum-decoration
            (butlast python-font-lock-keywords-maximum-decoration 2)))) ; remove assigment highlightings

  (font-lock-add-keywords
   'python-mode
   `((,(concat "\\(\\(?:\\(?:[_0-9A-Za-z]+\\.\\)?[_0-9A-Za-z]+\\)"
               "\\(?:\\s-*,\\s-*\\(?:[_0-9A-Za-z]+\\.\\)?[_0-9A-Za-z]+\\)*\\)"
               "\\(?:\\[.*?\\]\\)?\\s-+\\(?:=\\)\\s-+")
      (,(lambda (limit)
          (unless font-lock--skip
            (re-search-forward "\\(?:[_0-9A-Za-z]+\\.\\)?\\([_0-9A-Za-z]+\\)" limit t)))
       (progn
         (setq font-lock--skip nil)
         (goto-char (setq font-lock--anchor-beg-point (match-beginning 1)))
         (when (-some->> (syntax-ppss)
                         (-first-item)
                         (< 0))
           (setq font-lock--skip t))
         (match-end 1))
       (goto-char font-lock--anchor-beg-point)
       (1 (if (string= "_" (match-string-no-properties 1))
              'shadow
            'font-lock-variable-name-face))))
     ("\\([.:\\]\\)"
      (1 'shadow))
     ("[0-9A-Za-z]\\(_\\)[0-9A-Za-z]"
      (1 'symbol-dash-or-underline-face prepend)))
   :append)
  (font-lock-add-keywords
   'python-mode
   `(("\\(\\*\\*?\\)[_A-Za-z]"
      (1 'shadow))
     ("[A-Za-z]\\(=\\)\\(?:[\"'0-9A-Za-z]\\|\\s(\\)"
      (1 'shadow)))))

(use-package rpm-spec-mode
  :defer t
  :init
  (defface rpm-spec-changelog-item-face
    '((t (:inherit default)))
    "TODO")

  :config
  (font-lock-add-keywords
   'rpm-spec-mode
   '(("^%prein"
      (0 'rpm-spec-section-face))
     ("^%changelog$"
      ("^-.*$"
       (save-excursion
         (setq font-lock--anchor-beg-point (match-beginning 0))
         (if (re-search-forward "^%[-a-z]+$" nil t)
             (line-beginning-position)
           (point-max)))
       (goto-char font-lock--anchor-beg-point)
       (0 'rpm-spec-changelog-item-face t)))))
  (font-lock-add-keywords
   'rpm-spec-mode
   '(;; punctuation
     ("\\(#?'\\|[.,/<>]\\|\\s(\\|\\s)\\)"
      (1 'shadow append)))
   :append))

(use-package rust-mode
  :defer t
  :init
  (defface rust-self-var-face
    `((t (:inherit font-lock-keyword-face :weight ,(face-attribute 'default :weight))))
    "TODO")

  (defface rust-attribute-face
    `((t (:inherit font-lock-preprocessor-face)))
    "TODO")

  (defface rust-lifetimes-face
    `((t (:inherit font-lock-variable-name-face)))
    "TODO")

  (defface rust-punctuation-face
    '((t (:inherit shadow)))
    "TODO")

  :config
  (setq rust-font-lock-keywords
        (append
         `(
           ;; punctuation
           ("\\(\\_<_[0-9A-Za-z]*\\_>\\)" 1 'rust-punctuation-face)

           ;; Keywords proper
           (,(regexp-opt rust-keywords 'symbols) . font-lock-keyword-face)

           ;; Contextual keywords
           ("\\_<\\(default\\)[[:space:]]+fn\\_>" 1 font-lock-keyword-face)
           (,rust-re-union 1 font-lock-keyword-face)

           ;; Special types
           (,(regexp-opt rust-special-types 'symbols) . font-lock-type-face)

           ;; The unsafe keyword
           ("\\_<unsafe\\_>" . 'rust-unsafe-face)

           ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
           (,(rust-re-grab (concat "#\\!?\\[" rust-re-ident "[^]]*\\]"))
            1 'rust-attribute-face keep)

           ;; Builtin formatting macros
           (,(concat (rust-re-grab
                      (concat (rust-re-word (regexp-opt rust-builtin-formatting-macros))
                              "!"))
                     rust-formatting-macro-opening-re
                     "\\(?:" rust-start-of-string-re "\\)?")
            (1 'rust-builtin-formatting-macro-face)
            (rust-string-interpolation-matcher
             (rust-end-of-string)
             nil
             (0 'rust-string-interpolation-face t nil)))

           ;; write! macro
           (,(concat (rust-re-grab (concat (rust-re-word "write\\(ln\\)?") "!"))
                     rust-formatting-macro-opening-re
                     "[[:space:]]*[^\"]+,[[:space:]]*"
                     rust-start-of-string-re)
            (1 'rust-builtin-formatting-macro-face)
            (rust-string-interpolation-matcher
             (rust-end-of-string)
             nil
             (0 'rust-string-interpolation-face t nil)))

           ;; Syntax extension invocations like `foo!`, highlight including the !
           (,(concat (rust-re-grab (concat rust-re-ident "!")) "[({[:space:][]")
            1 font-lock-preprocessor-face)

           ;; Field names like `foo:`, highlight excluding the :
           (,(concat (rust-re-grab rust-re-ident) "[[:space:]]*:[^:]")
            1 font-lock-variable-name-face)

           ;; CamelCase Means Type Or Constructor
           (,rust-re-type-or-constructor 1 font-lock-type-face)

           ;; Type-inferred binding
           (,(concat "\\_<\\(?:let\\s-+ref\\|let\\|ref\\|for\\)\\s-+\\(?:mut\\s-+\\)?"
                     (rust-re-grab rust-re-ident)
                     "\\_>")
            1 font-lock-variable-name-face)

           ;; Type names like `Foo::`, highlight excluding the ::
           (,(rust-path-font-lock-matcher rust-re-uc-ident) 1 font-lock-type-face)

           ;; Module names like `foo::`, highlight excluding the ::
           (,(rust-path-font-lock-matcher rust-re-lc-ident) 1 font-lock-constant-face)

           ;; Lifetimes like `'foo`
           (,(concat "\\(&?'\\)" (rust-re-grab rust-re-ident) "[^']")
            (1 'rust-punctuation-face)
            (2 'rust-lifetimes-face))

           ;; Question mark operator
           ("\\?" . 'rust-question-mark-face)

           ;; Module names like `module::{foo, bar}`, highlight excluding the ::
           ("^\\s-*use\\s-+\\(?:[_0-9A-Za-z]+::\\)+\\s-*{"
            (,(lambda (limit)
                (unless font-lock--skip
                  (re-search-forward "\\_<[a-z][_0-9a-z]*\\_>" limit t)))
             (save-excursion
               (setq font-lock--skip nil
                     font-lock--anchor-beg-point (match-beginning 0))
               (safe-up-list-1)
               (point))
             (goto-char font-lock--anchor-beg-point)
             (0 font-lock-constant-face t)))

           ;; Lambda binding
           ("\\(|\\)\\([^\r\n|]+\\)\\(|\\)"
            (1 'rust-punctuation-face)
            (3 'rust-punctuation-face)
            ("\\_<\\([a-z][_0-9a-z]*\\)\\_>\\(?:\\s-*:\\s-*\\(?:[&*]+\\s-*\\)?[_0-9A-Za-z]+\\)?\\(?:\\s-*,\\)?"
             (progn
               (goto-char (setq font-lock--anchor-beg-point (match-beginning 2)))
               (match-end 2))
             (goto-char font-lock--anchor-beg-point)
             (1 'font-lock-variable-name-face)))

           ;; Pattern matched binding
           ("if let\\s-+[_:0-9A-Za-z]+\\s-*[({]\\(.+?\\)[})]\\s-*="
            ("\\([_0-9A-Za-z]+\\)"
             (progn
               (goto-char (match-beginning 1))
               (match-end 1))
             nil
             (1 'font-lock-variable-name-face)))
           ("match\\s-+.+?{$"
            (,(lambda (limit)
                (ignore-errors
                  (when font-lock--skip
                    (error ""))
                  (if (null font-lock--local-limit)
                      (when (re-search-forward "^\\s-*\\(.+?\\)\\s-*\\(?:|\\|=>\\)" limit t)
                        (goto-char (match-beginning 1))
                        (if (re-search-forward "[_:0-9A-Za-z]+\\s-*[({]\\s-*\\([_0-9A-Za-z]+\\)" (match-end 1) t)
                            (save-excursion
                              (safe-up-list-1)
                              (setq font-lock--local-limit (point)))
                          (set-match-data (fake-match-4)))
                        t)
                    (unless (re-search-forward "\\([_0-9A-Za-z]+\\)" font-lock--local-limit t)
                      (goto-char font-lock--local-limit)
                      (when (and (re-search-forward "\\s-*\\(?:|\\|=>\\)[ \t\r\n]*" limit t)
                                 (looking-at "\\s-*{"))
                        (goto-char (match-end 0))
                        (safe-up-list-1))
                      (setq font-lock--local-limit nil)
                      (set-match-data (fake-match-4)))
                    t)))
             (save-excursion
               (setq font-lock--skip nil
                     font-lock--anchor-beg-point (match-beginning 0)
                     font-lock--local-limit nil)
               (safe-up-list-1)
               (point))
             (goto-char font-lock--anchor-beg-point)
             (1 'font-lock-variable-name-face)))

           ;; punctuation
           ("\\([-=]>\\|::?\\|;\\)" 1 'rust-punctuation-face)
           ("\\(&\\|&?\\*+\\)\\(?:\\s(\\|[_0-9A-Za-z]\\)" 1 'rust-punctuation-face)
           )

         ;; Ensure we highlight `Foo` in `struct Foo` as a type.
         (mapcar #'(lambda (x)
                     (list (rust-re-item-def (car x))
                           1 (cdr x)))
                 '(("enum" . font-lock-type-face)
                   ("struct" . font-lock-type-face)
                   ("union" . font-lock-type-face)
                   ("type" . font-lock-type-face)
                   ("mod" . font-lock-constant-face)
                   ("use" . font-lock-constant-face)
                   ("fn" . font-lock-function-name-face))))))

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
       ("\\(\\$\\)[({]"
        (1 'shadow))
       ("\\([&|<>;]\\)"
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
             (setq-local font-lock--anchor-beg-point (match-beginning 0))
             (re-search-forward ,(regexp-quote " %>") (point-max))
             (match-string 0)))
         (goto-char font-lock--anchor-beg-point)
         (1 'web-mode-keyword-face)))
       (,begin-re
        ("\\([-_0-9A-Za-z]+\\)"
         (save-match-data
           (save-excursion
             (setq-local font-lock--anchor-beg-point (match-beginning 0))
             (re-search-forward ,end-re (point-max))
             (match-string 0)))
         (goto-char font-lock--anchor-beg-point)
         (1 'default))))
     :append)))

(use-package yaml-mode
  :defer t
  :config
  (font-lock-add-keywords
   'yaml-mode
   '(("\\(:\\)\\(\\s-\\|\\s-*$\\)"
      (1 'shadow)))))

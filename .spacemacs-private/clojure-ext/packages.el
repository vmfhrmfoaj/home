;;; packages.el --- clojure-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: KimJinseop <Jinseop@KimJinseops-iMac.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst clojure-ext-packages
  '(cider
    clj-refactor
    clojure-mode))

(defun clojure-ext/post-init-cider ()
  (use-package cider
    :defer t
    :config
    (byte-compile #'cider-connection-type-for-cljc-buffer)
    (byte-compile #'cider-cljs-root-dirs)
    (byte-compile #'cider-switch-to-releated-repl-buffer)
    (setq cider-font-lock-reader-conditionals nil
          cider-dynamic-indentation nil
          cider-font-lock-dynamically '(deprecated)
          cider-repl-display-in-current-window t
          cider-repl-use-pretty-printing t
          cider-mode-line '(:eval (when (cider-connected-p)
                                    "Ⓡ" ; (R)ELP
                                    ))
          cider-cljs-lein-repl (concat "(do"
                                       "  (require 'figwheel-sidecar.repl-api)"
                                       "  (figwheel-sidecar.repl-api/start-figwheel!)"
                                       "  (figwheel-sidecar.repl-api/cljs-repl))"))
    (evil-define-key 'insert cider-repl-mode-map (kbd "RET") #'evil-ret-and-indent)
    (evil-define-key 'normal cider-repl-mode-map (kbd "RET") #'cider-repl-return)
    (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
      "gn" #'cider-repl-set-ns
      "gN" #'cider-browse-ns)
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "en" #'cider-eval-ns-form))
    (advice-add #'cider-connection-type-for-buffer :before-until
                #'cider-connection-type-for-cljc-buffer)
    (advice-add #'cider-switch-to-repl-buffer :override
                #'cider-switch-to-releated-repl-buffer)
    (advice-add #'cider-expected-ns :around
                (byte-compile
                 (lambda (of &optional path)
                   (-if-let (root-dirs (and (cider-connected-p)
                                            (string= "cljs" (cider-connection-type-for-buffer))
                                            (cider-cljs-root-dirs)))
                       (let* ((path    (or path (file-truename (buffer-file-name))))
                              (relpath (->> root-dirs
                                            (--filter (string-prefix-p it path))
                                            (--sort (< (length it) (length other)))
                                            (-first-item))))
                         (when relpath
                           (->> path
                                (string-remove-prefix (concat relpath "/"))
                                (file-name-sans-extension)
                                (replace-regexp-in-string "/" ".")
                                (replace-regexp-in-string "_" "-"))))
                     (funcall of path)))))))

(defun clojure-ext/post-init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :config
    (with-eval-after-load 'diminish
      (diminish 'clj-refactor-mode))
    (setq cljr-clojure-test-declaration "[clojure.test :refer :all]"
          cljr-cljs-clojure-test-declaration "[cljs.test :refer-macros [async deftest is testing]]"
          cljr-clojure-test-namespace-under-test-alias "target"
          cljr-cljc-clojure-test-declaration
          (concat "#?(:clj  [clojure.test :refer :all]" "\n"
                  "   :cljs [cljs.test :refer [deftest is are] :include-macros true])")
          cljr-expectations-test-declaration "[expectations :refer :all]"
          cljr-favor-prefix-notation nil
          cljr-favor-private-functions nil
          cljr-inject-dependencies-at-jack-in nil ; for using custom version.
          cljr-prune-ns-form t ; for using the file local variable.
          cljr-suppress-middleware-warnings t
          cljr-warn-on-eval nil)
    (define-key clj-refactor-map (kbd "/") nil)
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "r/" (defalias 'cljr-magic-requires
               (lambda ()
                 (interactive)
                 (forward-char)
                 (cljr-slash)))))
    (add-hook 'clojure-mode-hook (-partial #'clj-refactor-mode 1))
    (add-hook 'cider-connected-hook
              (lambda ()
                (cider-nrepl-sync-request:eval
                 (concat "(alter-var-root #'refactor-nrepl.ns.rebuild/dependency-comparator" "\n"
                         "  (fn [f]"                                                         "\n"
                         "    (fn [d1 d2]"                                                   "\n"
                         "      (let [clj-regx #\"^(clojure|cljs)(\\.|\\s*\\[)\""            "\n"
                         "            expect-regx #\"^expectations\""                        "\n"
                         "            d1 (@#'refactor-nrepl.ns.rebuild/get-sort-name d1)"    "\n"
                         "            d2 (@#'refactor-nrepl.ns.rebuild/get-sort-name d2)]"   "\n"
                         "        (cond (and (re-find clj-regx d1)"                          "\n"
                         "                   (re-find clj-regx d2)) (.compareTo d1 d2)"      "\n"
                         "              (re-find clj-regx d1)    -1"                         "\n"
                         "              (re-find clj-regx d2)     1"                         "\n"
                         "              (re-find expect-regx d1) -1"                         "\n"
                         "              (re-find expect-regx d2)  1"                         "\n"
                         "              :else (.compareTo d1 d2))))))"))))
    (advice-add #'cljr--create-msg :filter-return
                (byte-compile
                 (lambda (msg)
                   "for missing a configuration."
                   (append msg (when (->> msg
                                          (-filter #'stringp)
                                          (-filter (-partial #'string= "prune-ns-form")))
                                 `("prune-ns-form" ,(if cljr-prune-ns-form "true" "false")))))))))

(defun clojure-ext/post-init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (byte-compile #'clojure--binding-regexp)
    (byte-compile #'clojure-skip)
    (byte-compile #'clojure-forward-sexp)

    (advice-add #'clojure-font-lock-extend-region-def :override
                (lambda (&rest _)
                  (-update->> font-lock-extend-region-functions
                              (remove 'clojure-font-lock-extend-region-def))))

    (let* ((whitespace  "[ \r\t\n]")
           (whitespace+ (concat whitespace "+"))
           (whitespace* (concat whitespace "*"))
           (symbol      clojure--sym-regexp)
           (symbol?     (concat "\\(?:" symbol "\\)?"))
           (namespace   (concat "\\(?:" symbol "/\\)"))
           (namespace?  (concat namespace "?"))
           (meta? "\\(?:\\(?:#\\^{[^}]*}\\|\\^:?\\sw+\\)[ \r\n\t]+\\)?")
           (core-ns  (concat (regexp-opt '("clojure.core" "cljs.core" "core") nil) "/"))
           (core-ns? (concat "\\(?:" core-ns "\\)?")))
      ;; TODO
      ;; I need a function to build anchored matches.
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
                          (setq-local font-lock--skip t)
                        (setq-local font-lock--skip nil)
                        (setq-local clojure-cond-form-point (point))
                        (safe-up-list-1)
                        (point)))
               (when (string-match-p "->>?" (match-string 1))
                 (condition-case nil
                     (clojure-forward-sexp)
                   (error (setq-local font-lock--skip t)))))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-cond-form-point))
             (0 'clojure-cond-condtion-face prepend)))
           (,(concat "(" core-ns? (regexp-opt '("if" "if-some" "if-let" "if-not")) "[ \r\t\n]+")
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
                   (setq-local font-lock--skip t)
                 (setq-local font-lock--skip nil)
                 (setq-local clojure-if-form-point (point))
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-if-form-point))
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
               (lambda (symbol limit)
                 (ignore-errors
                   (when font-lock--skip
                     (error ""))
                   (re-search-forward (concat "(\\(?:\\^" symbol "[ \r\t\n]+\\)?" symbol "\\>") limit)
                   (clojure-skip :vector)
                   (set-match-data
                    (if (looking-at-p "\"")
                        (list (point-marker) (progn (forward-sexp) (point-marker)))
                      (-repeat 2 (point-min-marker))))
                   t)))
              symbol)
            (save-excursion
              (if (in-comment?)
                  (setq-local font-lock--skip t)
                (setq-local font-lock--skip nil)
                (setq-local clojure-interface-form-point (point))
                (safe-up-list-1)
                (point)))
            (if font-lock--skip
                (end-of-line)
              (goto-char clojure-interface-form-point))
            (0 'font-lock-doc-face t))
           (,(-partial
              (byte-compile
               (lambda (symbol limit)
                 (when (re-search-forward (concat symbol "?\\(!+\\)\\>") limit 'no-err)
                   (let ((face (plist-get (text-properties-at (match-beginning 1)) 'face))
                         (ignore-faces '(font-lock-doc-face
                                         font-lock-string-face
                                         font-lock-comment-face)))
                     (when (memq face ignore-faces)
                       (set-match-data (-repeat 4 (point-min-marker))))
                     t))))
              symbol)
            (1 'clojure-side-effect-face t)))
         'append)
        ;; prepend rules
        (font-lock-add-keywords
         mode
         `(;; Binding forms
           (,(concat "(" core-ns? (substring (clojure--binding-regexp) 1))
            ;; Normal bindings
            (,(-partial
               (byte-compile
                (lambda (symbol namespace? limit)
                  (ignore-errors
                    (when font-lock--skip
                      (error ""))
                    (clojure-skip :comment :ignored-form :type-hint :destructuring-bind)
                    (let ((local-limit (save-excursion (forward-sexp) (point))))
                      (unless (and (re-search-forward (concat "\\<" namespace? "\\(" symbol "\\)\\>")
                                                      (min local-limit limit) t)
                                   (not (string-match-p clojure--ignore-binding-highlight-regex
                                                        (match-string-no-properties 1))))
                        (set-match-data (-repeat 4 (point-min-marker))))
                      (goto-char local-limit))
                    (clojure-forward-sexp)
                    t)))
               symbol namespace?)
             (save-excursion
               (if (in-comment?)
                   (setq-local font-lock--skip t)
                 (setq-local font-lock--skip nil)
                 (setq-local clojure-binding-form-point (point))
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-binding-form-point))
             (1 (if (and clojure-warning-if-pollute-core-namespace
                         (string-match-p clojure-core-regex (or (match-string 1) "")))
                    'clojure-local-binding-variable-name-warning-face
                  'clojure-local-binding-variable-name-face)))
            ;; Destructuring bindings
            (,(-partial
               (byte-compile
                (lambda (symbol limit)
                  ;; NOTE
                  ;; We need to iterate to search symbols in the destructuring form,
                  ;; but anchored-matcher does not support recursion.
                  (ignore-errors
                    (when font-lock--skip
                      (error ""))
                    (unless clojure-binding-form-recursive-point
                      (while (and (> limit (point))
                                  (prog1 t (clojure-skip :comment :ignored-form :type-hint))
                                  ;; skip normal bind?
                                  (not (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)"))
                                  (prog1 t (clojure-forward-sexp 2))))
                      (when (looking-at-p "[ \r\t\n]*\\(?:{\\|\\[\\)")
                        (setq-local clojure-binding-form-recursive-point (progn (down-list) (point)))
                        (setq-local clojure-binding-form-recursive-limit
                                    (save-excursion (up-list) (point)))))
                    (when clojure-binding-form-recursive-point
                      (clojure-skip :comment :ignored-form :type-hint)
                      (if (re-search-forward (concat "\\(?:\\^" symbol "[ \r\t\n]+\\)?" "\\(\\_<" symbol "\\>\\)")
                                             (min limit clojure-binding-form-recursive-limit) t)
                          (progn
                            ;; ignores
                            (when (string-match-p clojure--ignore-binding-highlight-regex
                                                  (match-string-no-properties 1))
                              (set-match-data (-repeat 4 (point-min-marker))))
                            ;; Handle default bind map
                            (when (save-excursion
                                    (backward-up-list)
                                    (and (char-after ?{)
                                         (ignore-errors
                                           (clojure-forward-sexp -1)
                                           (looking-at-p ":or"))))
                              (clojure-forward-sexp)))
                        (goto-char clojure-binding-form-recursive-limit)
                        (clojure-forward-sexp)
                        (setq-local clojure-binding-form-recursive-point nil)
                        (setq-local clojure-binding-form-recursive-limit nil)
                        (set-match-data (-repeat 4 (point-min-marker))))
                      t))))
               symbol)
             (save-excursion
               (if (in-comment?)
                   (setq-local font-lock--skip t)
                 (setq-local font-lock--skip nil)
                 (setq-local clojure-binding-form-point (point))
                 (setq-local clojure-binding-form-recursive-point nil)
                 (setq-local clojure-binding-form-recursive-limit nil)
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-binding-form-point))
             (1 (if (and clojure-warning-if-pollute-core-namespace
                         (string-match-p clojure-core-regex (or (match-string 1) "")))
                    'clojure-local-binding-variable-name-warning-face
                  'clojure-local-binding-variable-name-face))))
           ;; OOP style function forms & letfn
           (,(concat "(" core-ns?
                     "\\(?:"
                     (concat (regexp-opt '(;; "definterface"
                                           ;; "defprotocol"
                                           "defrecord"
                                           "deftype"
                                           "extend-protocol"
                                           "extend-type"
                                           "proxy"
                                           "reify"))
                             "[ \r\t\n]")
                     "\\|"
                     "letfn[ \r\t\n]+\\["
                     "\\)")
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
                        (if (re-search-forward (concat symbol "\\>") (min limit local-limit) t)
                            (add-to-list 'clojure-oop-fn-form-points (match-end 0))
                          (set-match-data (-repeat 2 (point-min-marker)))))
                      (up-list)
                      t))))
               symbol)
             (save-excursion
               (setq-local clojure-oop-fn-form-points nil)
               (if (in-comment?)
                   (setq-local font-lock--skip t)
                 (setq-local font-lock--skip nil)
                 (setq-local clojure-oop-fn-form-point (point))
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-oop-fn-form-point))
             (0 'clojure-semi-function-name-face))
            ;; OOP fn parameters highlight
            (,(-partial
               (byte-compile
                (lambda (symbol limit)
                  (ignore-errors
                    (when font-lock--skip
                      (error ""))
                    (unless clojure-oop-fn-recursive-point
                      (-when-let (point (car clojure-oop-fn-form-points))
                        (setq clojure-oop-fn-form-points (cdr clojure-oop-fn-form-points))
                        (goto-char point)
                        (re-search-forward "\\[" limit)
                        (setq clojure-oop-fn-recursive-point (point)
                              clojure-oop-fn-recursive-limit (save-excursion
                                                               (up-list)
                                                               (point)))))
                    (when clojure-oop-fn-recursive-point
                      (if (re-search-forward (concat "\\_<" symbol "\\>") (min limit clojure-oop-fn-recursive-limit) t)
                          (when (string-match-p clojure--ignore-binding-highlight-regex
                                                (match-string-no-properties 0))
                            (set-match-data (-repeat 2 (point-min-marker))))
                        (set-match-data (-repeat 2 (point-min-marker)))
                        (setq clojure-oop-fn-recursive-point nil
                              clojure-oop-fn-recursive-limit nil))
                      t))))
               symbol)
             (save-excursion
               (if (in-comment?)
                   (setq-local font-lock--skip t)
                 (setq-local font-lock--skip nil)
                 (setq-local clojure-oop-fn-form-point (point))
                 (setq-local clojure-oop-fn-recursive-point nil)
                 (setq-local clojure-oop-fn-recursive-limit nil)
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char clojure-oop-fn-form-point))
             (0 (if (and clojure-warning-if-pollute-core-namespace
                         (string-match-p clojure-core-regex (or (match-string 0) "")))
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
           (,(concat "(" core-ns? (regexp-opt '("defmacro"
                                                "defn"
                                                "defn-"
                                                "defmethod"
                                                "fn") t)
                     "\\>"
                     whitespace+
                     meta?
                     "\\(" symbol? "\\)")
            (1 'font-lock-keyword-face)
            ;; NOTE
            ;; Clojure is a Lisp-1.
            ;; It is mean the symbol of the variable and the function are no different.
            (2 (if (string-match-p "defmacro\\|^fn" (match-string 1))
                   'font-lock-function-name-face
                 'font-lock-variable-name-face))
            ;; fn parameters highlight
            (,(-partial
               (byte-compile
                (lambda (symbol limit)
                  (ignore-errors
                    (when font-lock--skip
                      (error ""))
                    (unless fn-recursive-point
                      (when fn-form-multi-arity?
                        (up-list 2))
                      (while (progn
                               (clojure-skip :comment :ignored-form :string :map)
                               (when fn-form-method?
                                 (setq fn-form-method? nil)
                                 (clojure-forward-sexp)
                                 t)))
                      (when (looking-at "(")
                        (setq fn-form-multi-arity? t)
                        (down-list))
                      (when (looking-at "\\[")
                        (down-list)
                        (setq fn-recursive-point (point)
                              fn-recursive-limit (save-excursion
                                                   (up-list)
                                                   (1- (point))))))
                    (when fn-recursive-point
                      (if (re-search-forward (concat "\\_<" symbol "\\>") (min limit fn-recursive-limit) t)
                          (when (string-match-p clojure--ignore-binding-highlight-regex (match-string 0))
                            (set-match-data (-repeat 2 (point-min-marker))))
                        (set-match-data (-repeat 2 (point-min-marker)))
                        (setq fn-recursive-point nil
                              fn-recursive-limit nil))
                      t))))
               symbol)
             (if (in-comment?)
                 (setq-local font-lock--skip t)
               (setq-local font-lock--skip nil)
               (setq-local fn-form-point (point))
               (setq-local fn-form-method? (string-match-p "defmethod" (match-string 1)))
               (setq-local fn-form-multi-arity? nil)
               (setq-local fn-recursive-point nil)
               (setq-local fn-recursive-limit nil)
               (save-excursion
                 (safe-up-list-1)
                 (point)))
             (if font-lock--skip
                 (end-of-line)
               (goto-char fn-form-point))
             (0 (if (and clojure-warning-if-pollute-core-namespace
                         (string-match-p clojure-core-regex (or (match-string 0) "")))
                    'clojure-fn-parameter-warning-face
                  'clojure-fn-parameter-face))))
           (,(concat "(" core-ns? "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                     whitespace+
                     meta?
                     "\\(" symbol "\\)")
            (1 'font-lock-keyword-face)
            (2 'font-lock-variable-name-face))
           ("\\<%[&1-9]?\\>"
            (0 'clojure-special-variable-name-face))
           (,(concat "\\(?:\\<\\|/\\)@?"
                     "\\(\\*[^" clojure--sym-forbidden-rest-chars "*]*\\*\\)\\>")
            (1 'clojure-special-variable-name-face))
           (,(concat "(" (regexp-opt '("->"     "->>"
                                       "as->"   "as->>"
                                       "some->" "some->>"
                                       "and"
                                       "or") t))
            (1 'default))
           ;; Adds rules
           (,(concat "\\^\\(" symbol "\\)\\>")
            (1 'font-lock-type-face))
           (,(concat "\\(#js\\)"
                     whitespace*
                     "\\s(")
            (1 'font-lock-builtin-face))
           (,(concat "(" namespace?
                     (regexp-opt '("go-loop"
                                   "with-hard-redefs")
                                 t))
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
           (,(concat "("
                     (regexp-opt '("case"
                                   "cond" "condp" "cond->" "cond->>"
                                   "for"
                                   "if" "if-let" "if-not"
                                   "recur"
                                   "throw"
                                   "when" "when-let" "when-not"
                                   "while"
                                   ) t)
                     "\\(?:)\\|" whitespace "\\)")
            (1 'clojure-important-keywords-face))))))

    (setq clojure-indent-style :align-arguments)
    (put 'defstate 'clojure-doc-string-elt 2)
    (put-clojure-indent 'redef-state :defn) ; for expectations
    (put-clojure-indent 'fdef :defn) ; for spec
    (eval-after-load "page-break-lines"
      '(add-to-list 'page-break-lines-modes 'clojure-mode))
    (add-hook
     'clojure-mode-hook
     (lambda ()
       (setq-local custom-forward-symbol
                   (byte-compile
                    (lambda (n)
                      (let ((sym     (concat "^/" clojure--sym-forbidden-rest-chars))
                            (not-sym (concat "/"  clojure--sym-forbidden-rest-chars))
                            (skip-chars (if (< 0 n)
                                            (lambda (s)
                                              (skip-chars-forward s)
                                              (skip-chars-backward "."))
                                          (lambda (s)
                                            (skip-chars-backward s)
                                            (if (looking-at-p "\\.-?")
                                                (skip-chars-forward ".-")))))
                            (n (abs n)))
                        (while (<= 1 n)
                          (setq n (1- n))
                          (funcall skip-chars not-sym)
                          (funcall skip-chars sym))))))
       (let ((file-name (or (buffer-file-name) ""))
             (ns-form   (save-match-data
                          (save-excursion
                            (if (clojure-find-ns)
                                (progn
                                  (goto-char (match-beginning 0))
                                  (end-of-defun)
                                  (let ((end (point)))
                                    (backward-sexp 1)
                                    (buffer-substring-no-properties (point) end)))
                              ""))))
             (compojure-kws '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH" "ANY" "context"))
             keywords)
         (cond
          ((string-match-p "_test.clj[cs]?$" file-name)
           (add-to-list 'keywords "async")
           (add-to-list 'keywords "is"))
          ((string-match-p "_expectations.clj[cs]?$" file-name)
           (make-local-variable 'font-lock-keywords)
           (font-lock-add-keywords
            nil
            '(("(\\(expect\\|freeze-time\\)[ \r\t\n]"
               (1 'font-lock-keyword-face))))
           (add-to-list 'keywords "expect")
           (add-to-list 'keywords "freeze-time"))
          ((string-match-p (concat "compojure.core :"
                                   "\\(?:as"
                                   "\\|refer[ \r\t\n]\\[[^]]*"
                                   (regexp-opt compojure-kws)
                                   "\\>\\)")
                           ns-form)
           (setq keywords (append keywords compojure-kws))))
         (when keywords
           (setq-local clojure-get-indent-function
                       (-partial
                        (byte-compile
                         (lambda (keywords func-name)
                           (and (string-match-p keywords func-name) :defn)))
                        (regexp-opt keywords)))))))
    (advice-add #'clojure-font-lock-syntactic-face-function :around
                (lambda (of state)
                  "(def _ \"abc\")<~ It should be the string."
                  (let ((res (funcall of state)))
                    (if (and (eq res font-lock-doc-face)
                             (save-excursion
                               (goto-char (nth 1 state))
                               (not (looking-at-p "(defprotocol\\>")))
                             (let ((list-end (save-excursion
                                               (goto-char (nth 1 state))
                                               (forward-sexp)
                                               (1- (point))))
                                   (str-end  (save-excursion
                                               (goto-char (nth 8 state))
                                               (forward-sexp)
                                               (point))))
                               (= list-end str-end)))
                        font-lock-string-face
                      res))))))

;;; packages.el ends here

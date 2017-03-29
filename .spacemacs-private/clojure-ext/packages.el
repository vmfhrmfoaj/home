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
                                       "  (swap! @#'strictly-specking-standalone.core/registry-ref"
                                       "         dissoc :figwheel-sidecar.schemas.cljs-options/compiler-options)"
                                       "  (strictly-specking-standalone.core/def-key"
                                       "    :figwheel-sidecar.schemas.cljs-options/compiler-options"
                                       "    map?)"
                                       "  (figwheel-sidecar.repl-api/start-figwheel!)"
                                       "  (figwheel-sidecar.repl-api/cljs-repl))"))
    (evil-define-key 'insert cider-repl-mode-map (kbd "RET") #'evil-ret-and-indent)
    (evil-define-key 'normal cider-repl-mode-map (kbd "RET") #'cider-repl-return)
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "en" #'cider-eval-ns-form))
    (advice-add #'cider-connection-type-for-buffer :before-until
                #'cider-connection-type-for-cljc-buffer)
    (advice-add #'cider-expected-ns :around
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
                    (funcall of path))))))

(defun clojure-ext/post-init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :config
    (with-eval-after-load 'diminish
      (diminish 'clj-refactor-mode))
    (setq cljr-clojure-test-declaration "[clojure.test :refer :all]"
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
                (lambda (msg)
                  "for missing a configuration."
                  (append msg (when (->> msg
                                         (-filter #'stringp)
                                         (-filter (-partial #'string= "prune-ns-form")))
                                `("prune-ns-form" ,(if cljr-prune-ns-form "true" "false"))))))
    (with-eval-after-load 'smartparens
      (advice-add #'cljr-slash :after
                  (let ((byte-compile-warnings nil)
                        (byte-compile-dynamic t)
                        (f (lambda ()
                             (when (and (not (bound-and-true-p multiple-cursors-mode))
                                        (not (sp-point-in-string-or-comment))
                                        (->> (char-before (1- (point)))
                                             (char-to-string)
                                             (string-match-p "[0-9A-Za-z]")))
                               (company-cancel)
                               (company-complete-common-or-cycle)))))
                    (byte-compile f))))))

(defun clojure-ext/post-init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (let* ((whitespace  "[ \r\t\n]")
           (whitespace+ (concat whitespace "+"))
           (whitespace* (concat whitespace "*"))
           (symbol      (concat clojure--sym-regexp "?"))
           (symbol?     (concat "\\(?:" symbol "\\)?"))
           (namespace   (concat "\\(?:" symbol "/\\)"))
           (namespace?  (concat namespace "?"))
           (namespace*  (concat namespace "*"))
           (meta* "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"))
      (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
        (font-lock-add-keywords
         mode
         `(;; Removes(overwrite) a rules
           (,(concat "(" namespace?
                     "\\(default/?[^" clojure--sym-forbidden-rest-chars "]*\\)"
                     whitespace*
                     "\\(" symbol? "\\)\\>")
            (1 'default)
            (2 'default))
           (,(concat "(" namespace?
                     "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                     whitespace+
                     meta*
                     "::?" namespace* "\\(" symbol "\\)\\>")
            (1 'font-lock-keyword-face)
            (2 'clojure-defining-spec-face))
           (,(concat "(" namespace?
                     (regexp-opt '("defn"
                                   "defn-"
                                   "defmacro"
                                   "defmethod") t)
                     "\\>"
                     whitespace+
                     meta*
                     "\\(" symbol? "\\)\\(!*\\)\\>"
                     whitespace+)
            (1 'font-lock-keyword-face)
            (2 'font-lock-function-name-face)
            (3 'clojure-side-effect-face))
           (,(concat "(" namespace?
                     "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                     whitespace+
                     meta*
                     "\\(" symbol "?\\)\\(!*\\)\\>"
                     whitespace+)
            (1 'font-lock-keyword-face)
            (2 'font-lock-variable-name-face)
            (3 'clojure-side-effect-face))
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
           ;; Adds a rules
           (,(concat symbol "?\\(!+\\)\\>")
            (1 'clojure-side-effect-face))
           (,(concat "\\(#js\\)"
                     whitespace*
                     "\\s(")
            (1 'font-lock-builtin-face))
           (,(concat "(" namespace?
                     (regexp-opt '("extend-protocol"
                                   "go-loop") t))
            (1 'font-lock-keyword-face))
           (,(concat "(ns"
                     whitespace+
                     meta*
                     "\\(" symbol "\\)\\>")
            (1 'clojure-defining-ns-face))
           (,(concat "\\<\\(\\.-?\\)[_a-z][-_a-zA-Z0-9]*\\>")
            (1 'font-lock-keyword-face))
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
    (evil-define-key 'insert clojure-mode-map (kbd "SPC") #'clojure-space-key)
    (eval-after-load "page-break-lines"
      '(add-to-list 'page-break-lines-modes 'clojure-mode))
    (add-hook
     'clojure-mode-hook
     (lambda ()
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
             keywords)
         (when (string-match-p "_expectations.clj[cs]?" file-name)
           (make-local-variable 'font-lock-keywords)
           (font-lock-add-keywords
            nil
            '(("(\\(expect\\|freeze-time\\)[ \r\t\n]"
               (1 'font-lock-keyword-face))))
           (add-to-list 'keywords "expect")
           (add-to-list 'keywords "freeze-time"))
         (when (string-match-p (concat "compojure.core :"
                                       "\\(?:as"
                                       "\\|refer[ \r\t\n]\\[[^]]*"
                                       (regexp-opt '("GET"
                                                     "POST"
                                                     "PUT"
                                                     "DELETE"
                                                     "HEAD"
                                                     "OPTIONS"
                                                     "PATCH"
                                                     "ANY"
                                                     "context"))
                                       "\\>\\)")
                               ns-form)
           (setq keywords (append keywords '("GET"
                                             "POST"
                                             "PUT"
                                             "DELETE"
                                             "HEAD"
                                             "OPTIONS"
                                             "PATCH"
                                             "ANY"
                                             "context"))))
         (when keywords
           (setq-local clojure-get-indent-function
                       (lexical-let ((keywords (regexp-opt keywords)))
                         (lambda (func-name)
                           (and (string-match-p keywords func-name) :defn))))))))))

;;; packages.el ends here

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
    (setq cider-dynamic-indentation nil
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
        "en" #'cider-eval-ns-form))))

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
          cljr-prune-ns-form nil
          cljr-favor-private-functions nil)
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
                         "                   (re-find clj-regx d1) -1"                       "\n"
                         "                   (re-find clj-regx d2) 1"                        "\n"
                         "                   (re-find expect-regx d1) -1"                    "\n"
                         "                   (re-find expect-regx d2) 1"                     "\n"
                         "                   :else (.compareTo d1 d2))))))"))))
    (advice-add #'cljr--create-msg :filter-return
                (lambda (msg)
                  "for missing a configuration."
                  (append msg `("prune-ns-form" ,(if cljr-prune-ns-form "true" "false")))))
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
           (symbol      clojure--sym-regexp)
           (namespace   (concat "\\(?:" clojure--sym-regexp "/\\)"))
           (namespace?  (concat namespace "?"))
           (meta* "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"))
      (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
        (font-lock-add-keywords
         mode
         `(;; Replaces a regex rules `clojure-mode'.
           (,(concat "(" namespace?
                     "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                     whitespace+
                     meta*
                     "\\(:\\(?::\\|" namespace "\\)" symbol "\\)"
                     )
            (1 'font-lock-keyword-face)
            (2 'clojure-defining-spec-face))
           (,(concat "(" namespace?
                     (regexp-opt '("defn" "defmacro") t) "\\>"
                     whitespace+
                     meta*
                     "\\(" symbol "?\\)\\(!*\\)"
                     whitespace+)
            (1 'font-lock-keyword-face)
            (2 'font-lock-function-name-face)
            (3 'clojure-side-effect-face))
           (,(concat "(" namespace?
                     "\\(default\\(?:/?" symbol "\\)?\\)\\>"
                     whitespace+)
            (1 'default))
           (,(concat "(" namespace?
                     "\\(def[^" clojure--sym-forbidden-rest-chars "]*\\)\\>"
                     whitespace+
                     meta*
                     "\\(" symbol "?\\)\\(!*\\)"
                     whitespace+)
            (1 'font-lock-keyword-face)
            (2 'font-lock-variable-name-face)
            (3 'clojure-side-effect-face))
           ;; Adds a rules
           (,(concat symbol "\\(!\\)\\(?:" whitespace "\\|)\\)")
            (1 'clojure-side-effect-face))
           (,(concat "\\(#js\\)"
                     whitespace*
                     "\\s(")
            (1 'font-lock-builtin-face))
           (,(regexp-opt '("extend-protocol"
                           "go-loop") t)
            (1 'font-lock-builtin-face))
           (,(concat "(ns"
                     whitespace+
                     meta*
                     "\\(" symbol "\\)")
            (1 'clojure-defining-ns-face))
           (,(concat "\\_<\\(\\.-?\\)[_a-z][-_a-zA-Z0-9]*\\_>")
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
            (1 'clojure-important-keywords-face))
           ;; Removes(overwrite) a rules
           (,(concat "(" (regexp-opt '("->"     "->>"
                                       "as->"   "as->>"
                                       "cond->" "cond->>"
                                       "some->" "some->>"
                                       "and"
                                       "or") t))
            (1 'default))))))
    (setq clojure-indent-style :align-arguments)
    (put 'defstate 'clojure-doc-string-elt 2)
    (put-clojure-indent 'redef-state :defn) ; for expectations
    (add-hook 'clojure-mode-hook
              (lambda ()
                (when (string-match-p "_expectations.clj\\(?:c\\|s\\)?$" (buffer-file-name))
                  (setq-local clojure-get-indent-function
                              (lambda (fn)
                                (when (string-match-p "\\(?:expect\\|freeze-time\\)" fn)
                                  1))))))))

;;; packages.el ends here

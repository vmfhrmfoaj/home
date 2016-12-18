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
          cider-repl-use-pretty-printing t
          cider-mode-line
          '(:eval (when (cider-connected-p)
                    (with-current-buffer (ignore-errors (cider-current-connection))
                      (format " CIDER<%s%s>"
                              (let ((host (car nrepl-endpoint)))
                                (if (string-equal host "localhost")
                                    ""
                                  (concat host ":")))
                              (cadr nrepl-endpoint)))))
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
          cljr-auto-clean-ns nil)
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
    (with-eval-after-load 'smartparens
      (advice-add #'cljr-slash :after
                  (lambda ()
                    (when (and (not (bound-and-true-p multiple-cursors-mode))
                               (not (sp-point-in-string-or-comment))
                               (->> (char-before (1- (point)))
                                    (char-to-string)
                                    (string-match-p "[0-9A-Za-z]")))
                      (company-cancel)
                      (company-complete-common-or-cycle)))))))

(defun clojure-ext/post-init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      (font-lock-add-keywords
       mode
       '(("\\s(\\(?:[^ \r\t\n]+?/\\)?\\(default[^ \r\t\n]*?\\)[ \t\n]+\\([^ \r\t\n]+?\\)"
          (1 'default)
          (2 'default))
         ("\\s(\\(\\(?:as\\|cond\\|some\\)?->>?\\|and\\|or\\)\\_>"
          1 'default)
         ("^\\s-*\\s(def[ \r\n\t]+\\([^ \r\t\n]+?\\)\\(!+\\)[ \r\t\n]"
          (1 'font-lock-variable-name-face)
          (2 'clojure-side-effect-face))
         ("^\\s-*\\s(defn-?[ \r\n\t]+\\([^ \r\t\n]+?\\)\\(!+\\)[ \r\t\n]"
          (1 'font-lock-function-name-face)
          (2 'clojure-side-effect-face))
         ("\\(!+\\)\\(?:\\s-+\\|\\s)\\|$\\)"
          1 'clojure-side-effect-face t)
         ("\\(#js\\)\\s-*\\s("
          1 'font-lock-builtin-face)
         ("\\_<\\(\\.-?\\)[a-z][a-zA-Z0-9]*\\_>"
          1 'font-lock-keyword-face)
         ("(\\(extend-protocol\\|go-loop\\|while\\)[ \r\t\n]"
          1 'font-lock-keyword-face)
         ("(\\(?:defstate\\|defproject\\)[ \r\t\n]+\\([^ \r\t\n]+\\)[ \r\t\n]"
          1 'font-lock-variable-name-face))))
    (setq clojure-indent-style :align-arguments)
    (put 'defstate 'clojure-doc-string-elt 2)
    (add-hook 'clojure-mode-hook
              (lambda ()
                (when (string-match-p "_expectations.clj\\(?:c\\|s\\)?" (buffer-file-name))
                  (make-local-variable 'font-lock-keywords)
                  (font-lock-add-keywords
                   nil
                   '(("(\\(expect\\)[ \r\t\n]"
                      1 '(:inherit font-lock-keyword-face :weight bold))))
                  (setq-local clojure-get-indent-function
                              (lambda (fn)
                                (when (string-match-p "\\(?:expect\\|freeze-time\\)" fn)
                                  1))))))))

;;; packages.el ends here

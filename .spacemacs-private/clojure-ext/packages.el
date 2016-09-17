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
  '(
    cider
    clj-refactor
    clojure-mode
    ))

(defun clojure-ext/post-init-cider ()
  (use-package cider
    :config
    (setq cider-repl-use-pretty-printing t
          cider-mode-line
          '(:eval (with-current-buffer (ignore-errors (cider-current-connection))
                    (format " CIDER<%s%s>"
                            (let ((host (car nrepl-endpoint)))
                              (if (string-equal host "localhost")
                                  ""
                                (concat host ":")))
                            (cadr nrepl-endpoint)))))
    (evil-define-key 'insert cider-repl-mode-map (kbd "RET") #'evil-ret-and-indent)
    (evil-define-key 'normal cider-repl-mode-map (kbd "RET") #'cider-repl-return)))

(defun clojure-ext/post-init-clj-refactor ()
  (use-package clj-refactor
    :config
    (eval-after-load "diminish" '(diminish 'clj-refactor-mode))
    (setq cljr-expectations-test-declaration "[expectations :refer :all]")
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
    (eval-after-load "smartparens"
      '(advice-add #'cljr-slash :after
                   (lambda ()
                     (when (and (not (bound-and-true-p multiple-cursors-mode))
                                (not (sp-point-in-string-or-comment))
                                (->> (char-before (1- (point)))
                                     (char-to-string)
                                     (string-match-p "[0-9A-Za-z]")))
                       (company-complete-common-or-cycle)))))))

(defun clojure-ext/post-init-clojure-mode ()
  (use-package clojure-mode
    :config
    (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
      (font-lock-add-keywords
       mode
       '(("\\s(\\(?:[^ \r\t\n]+?/\\)?\\(default[^ \r\t\n]*?\\)[ \t\n]+\\([^ \r\t\n]+?\\)"
          (1 '(:inherit default))
          (2 '(:inherit default)))
         ("\\s(\\(\\(?:as\\|some\\)?->>?\\|and\\|or\\)\\_>"
          1 '(:inherit default))
         ("^\\s-*\\s(def-[ \r\n\t]\\([^ \r\t\n]+\\)"
          1 '(:inherit font-lock-variable-name-face) nil)
         ("^\\s-*\\s(def-?[ \r\n\t]+\\([^ \r\t\n]+?\\)\\(!+\\)[ \r\t\n]"
          (1 '(:inherit font-lock-variable-name-face))
          (2 '(:inherit font-lock-warning-face :slant italic)))
         ("^\\s-*\\s(defn-?[ \r\n\t]+\\([^ \r\t\n]+?\\)\\(!+\\)[ \r\t\n]"
          (1 '(:inherit font-lock-function-name-face))
          (2 '(:inherit font-lock-warning-face :slant italic)))
         ("\\(#js\\)\\s-+\\s("
          1 '(:inherit font-lock-builtin-face))
         ("\\_<\\(\\.-?\\)[a-z][a-zA-Z0-9]*\\_>"
          1 '(:inherit font-lock-keyword-face))))
      (font-lock-add-keywords
       mode
       '(("\\(!+\\)\\(?:\\s-+\\|\\s)\\|$\\)"
          1 '(:inherit font-lock-warning-face :slant italic))) t))
    (setq clojure-indent-style :align-arguments)
    (put 'def-      'clojure-doc-string-elt 2)
    (put 'defmacro- 'clojure-doc-string-elt 2)))

;;; packages.el ends here

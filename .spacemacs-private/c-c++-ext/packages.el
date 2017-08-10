;;; packages.el --- c-c++-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `c-c++-ext-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `c-c++-ext/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `c-c++-ext/pre-init-PACKAGE' and/or
;;   `c-c++-ext/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst c-c++-ext-packages
  '(cc-mode))

(defun c-c++-ext/post-init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    ;; for Linux
    (font-lock-add-keywords
     'c-mode
     '(("[_0-9a-zA-Z]+\\s-+__[_0-9a-zA-Z]+\\s-+\\([_0-9a-zA-Z]+\\)("
        (1 '(:inherit font-lock-function-name-face)))))
    ;; for objc
    (font-lock-add-keywords
     'objc-mode
     '(("\\(@\\(?:property\\|synthesize\\|dynamic\\)\\)"
        1 '(:inherit font-lock-keyword-face))))
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (and buffer-file-name
                           (string-match-p "\\.h$" buffer-file-name)
                           (not (eq 'objc-mode major-mode)))
                  (save-excursion
                    (beginning-of-buffer)
                    (when (re-search-forward "^\\s-*@\\(?:class\\|interface\\|end\\)" nil t)
                      (objc-mode))))
                (let ((level 1))
                  (c-set-offset 'access-label          (* level -1))
                  (c-set-offset 'brace-list-intro      (* level 2))
                  (c-set-offset 'defun-block-intro     (* level 2))
                  (c-set-offset 'inclass               (* level 2))
                  (c-set-offset 'inextern-lang         0)
                  (c-set-offset 'innamespace           0)
                  (c-set-offset 'label                 0)
                  (c-set-offset 'statement-block-intro (* level 2))
                  (c-set-offset 'statement-case-intro  (* level 2))
                  (c-set-offset 'substatement          (* level 2))
                  (c-set-offset 'substatement-open     0)
                  (c-set-offset 'case-label            level)
                  (c-set-offset 'statement-case-intro  level)
                  (c-set-offset 'statement-case-open   0)
                  (c-set-offset 'member-init-intro     (* level 2))
                  (c-set-offset 'statement-cont        (* level 2)))))
    (add-hook 'c++-mode-hook
              (lambda ()
                (define-key c++-mode-map [tab] 'clang-format-buffer)))))

;;; packages.el ends here

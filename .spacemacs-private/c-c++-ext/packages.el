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
    (spacemacs/declare-prefix-for-mode 'c-mode "mh" "help")
    (spacemacs/set-leader-keys-for-major-mode 'c-mode
      "hh" #'helm-man-woman)
    ;; for Linux
    (font-lock-add-keywords
     'c-mode
     '(("[_0-9a-zA-Z]+\\s-+\\(__[_0-9a-zA-Z]+\\)\\s-+\\([_0-9a-zA-Z]+\\)\\s-*("
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face))))
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
                ;; NOTE
                ;; You can apply different indent styles to each project as following:
                ;; .dir-locals: ((c-mode . ((eval . (c-setup-indent-config c-indent-config--xxx)))))
                (c-setup-indent-config c-indent-config)))))

;;; packages.el ends here

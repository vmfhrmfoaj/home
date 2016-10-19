;;; packages.el --- spacemacs-editing-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst spacemacs-editing-ext-packages
  '((isearch-mode :location built-in)
    smartparens))

(defun spacemacs-editing-ext/init-isearch-mode ()
  (define-key isearch-mode-map (kbd "C-h") #'isearch-delete-char)
  (define-key isearch-mode-map (kbd "SPC") (isearch-fn ".*?" " "))
  (define-key isearch-mode-map (kbd "M-<") (isearch-fn "\\_<"))
  (define-key isearch-mode-map (kbd "M->") (isearch-fn "\\_>")))

(defun spacemacs-editing-ext/post-init-smartparens()
  (use-package smartparens
    :defer t
    :config
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          wrap-sp-supported-modes '(clojure-mode
                                    cider-repl-mode
                                    emas-lisp-mode))
    (define-key evil-lisp-state-map (kbd "C-w") #'sp-rewrap-sexp)
    (advice-add #'sp-newline :after #'auto-indent)
    (when (require 'smartparens-clojure nil t)
      (advice-add #'sp-forward-symbol :before #'wrap-sp-forward-symbol)
      (advice-add #'sp-backward-symbol :after #'wrap-sp-backward-symbol)
      (advice-add #'sp-forward-sexp :after #'wrap-sp-forward-sexp)
      (advice-add #'sp-backward-sexp :after #'wrap-sp-backward-sexp))))

;;; packages.el ends here

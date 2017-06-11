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
  (setq isearch-mode-end-hook-quit t)
  (add-hook 'isearch-mode-end-hook #'deactivate-input-method)
  (define-key isearch-mode-map (kbd "C-h") #'isearch-delete-char)
  (define-key isearch-mode-map (kbd "SPC") (isearch-fn ".*?" " "))
  (define-key isearch-mode-map (kbd "M-<") (isearch-fn "\\_<"))
  (define-key isearch-mode-map (kbd "M->") (isearch-fn "\\_>"))
  (advice-add #'isearch-update :after
              (let ((byte-compile-warnings nil)
                    (byte-compile-dynamic t))
                (byte-compile
                 (lambda (&rest _)
                   (if (and isearch-message (not (string-empty-p isearch-message)))
                       (recenter)))))))

(defun spacemacs-editing-ext/post-init-smartparens()
  (use-package smartparens
    :defer t
    :config
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          sp-show-pair-from-inside nil
          wrap-sp-supported-modes '(clojure-mode
                                    cider-repl-mode
                                    emas-lisp-mode))
    (define-key evil-lisp-state-map (kbd "C-w") #'sp-rewrap-sexp)
    (advice-add #'sp-newline :after #'auto-indent)))

;;; packages.el ends here

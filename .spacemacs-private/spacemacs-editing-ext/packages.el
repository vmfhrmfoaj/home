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
  (define-key isearch-mode-map (kbd "C-k") #'isearch-repeat-backward) ; evil convention
  (define-key isearch-mode-map (kbd "C-j") #'isearch-repeat-forward)  ; evil convention
  (advice-add #'isearch-update :after
              (let ((byte-compile-warnings nil)
                    (byte-compile-dynamic t))
                (byte-compile
                 (lambda (&rest _)
                   (if (and isearch-message (not (string-empty-p isearch-message)))
                       (recenter))))))
  (add-hook 'isearch-mode-hook
            (let ((byte-compile-warnings nil)
                  (byte-compile-dynamic t))
              (byte-compile
               (lambda ()
                 (if (region-active-p)
                     (let* ((s (region-beginning))
                            (e (1+ (region-end))) ; ???
                            (str   (buffer-substring-no-properties s e))
                            (regex (regexp-quote str)))
                       (deactivate-mark)
                       (setq isearch-string  regex
                             isearch-message regex)
                       (isearch-search-and-update))))))))

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
    (advice-add #'sp-newline :after #'auto-indent)))

;;; packages.el ends here

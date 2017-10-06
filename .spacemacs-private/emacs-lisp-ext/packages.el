;;; packages.el --- emacs-lisp-ext layer packages file for Spacemacs.
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

(defconst emacs-lisp-ext-packages
  '((emacs-lisp :location built-in)))

(defun emacs-lisp-ext/post-init-emacs-lisp ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
      1 'default nil)
     ("(\\(lexical-let\\*?\\)"
      1 'font-lock-keyword-face)
     ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
      1 'font-lock-constant-face)
     ("(\\(assert\\)"
      1 'font-lock-warning-face)
     (" \\(\\?.\\)"
      1 'font-lock-string-face)
     ;; spacemacs style function
     ("(defun[ \r\t\n]+\\([-/=>&?0-9a-zA-Z]+\\)"
      1 'font-lock-function-name-face t)))
  (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
  (with-eval-after-load 'evil
    (let ((f (byte-compile
              (lambda ()
                (interactive)
                (beginning-of-defun)
                (forward-list)
                (eval-print-last-sexp)))))
      (define-key lisp-interaction-mode-map [remap eval-print-last-sexp]  f)
      (evil-define-key 'normal lisp-interaction-mode-map [remap evil-ret] f)))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "ss" (defalias 'jump-to-emacs-lisp-REPL
           (lambda ()
             (interactive)
             (switch-to-buffer (get-buffer-create "*emacs-lisp-REPL*"))
             (lisp-interaction-mode)))))

;;; packages.el ends here

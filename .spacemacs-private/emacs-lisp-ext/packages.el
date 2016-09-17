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
  '(
    (emacs-lisp :location built-in)
    ))

(defun emacs-lisp-ext/post-init-emacs-lisp ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
      1 '(:inherit default) nil)
     ("(\\(lexical-let\\*?\\)"
      1 '(:inherit font-lock-keyword-face))
     ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
      1 '(:inherit font-lock-constant-face))
     ("(\\(assert\\)"
      1 '(:inherit font-lock-warning-face))
     (" \\(\\?.\\)"
      1 '(:inherit font-lock-string-face))
     ("\\(:[-+*/?0-9A-Za-z]+\\)"
      1 '(:inherit font-lock-constant-face))))
  (eval-after-load "evil"
    '(progn
       (evil-define-key 'insert lisp-interaction-mode-map (kbd "RET") #'evil-ret-and-indent)
       (evil-define-key 'insert lisp-interaction-mode-map (kbd "C-j") #'evil-ret-and-indent)
       (let ((f (lambda ()
                  (interactive)
                  (beginning-of-defun)
                  (forward-list)
                  (eval-print-last-sexp))))
         (evil-define-key 'normal lisp-interaction-mode-map (kbd "RET") f)
         (evil-define-key 'normal lisp-interaction-mode-map (kbd "C-j") f)))))

;;; packages.el ends here

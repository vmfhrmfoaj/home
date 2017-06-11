;;; packages.el --- spacemacs-editing-visual-ext layer packages file for Spacemacs.
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

(defconst spacemacs-editing-visual-ext-packages
  '(auto-highlight-symbol
    rainbow-delimiters))

(defun spacemacs-editing-visual-ext/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :config
    (setq ahs-include '((clojure-mode . "[^ \r\t\n]+")
                        (clojurescript-mode . "[^ \r\t\n]+")
                        (clojurec-mode . "[^ \r\t\n]+")
                        (emacs-lisp-mode . "[^ \r\t\n]+")))
    (advice-add #'ahs-select :after
                (let ((byte-compile-warnings nil)
                      (byte-compile-dynamic t)
                      (f (byte-compile(lambda (&rest _) (recenter)))))))))

(defun spacemacs-editing-visual-ext/post-init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :config
    (setq rainbow-delimiters--prefix-str (concat "#" "_" "'" "`"))
    (advice-add #'rainbow-delimiters--apply-color :override
                (let ((byte-compile-warnings nil)
                      (byte-compile-dynamic t)
                      (f (lambda (loc depth match)
                           (let ((face (funcall rainbow-delimiters-pick-face-function depth match loc))
                                 (start loc)
                                 (end (1+ loc)))
                             (when face
                               (save-excursion
                                 (goto-char start)
                                 (when (looking-at-p "\\s(")
                                   (skip-chars-backward rainbow-delimiters--prefix-str)
                                   (setq start (point))))
                               (font-lock-prepend-text-property start end 'face face))))))
                  (byte-compile f)))))

;;; packages.el ends here

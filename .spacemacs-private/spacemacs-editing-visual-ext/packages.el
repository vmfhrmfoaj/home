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
   '(auto-highlight-symbol))

(defun spacemacs-editing-visual-ext/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :config
    (setq ahs-include '((clojure-mode . "[^ \r\t\n]+")
                        (clojurescript-mode . "[^ \r\t\n]+")
                        (clojurec-mode . "[^ \r\t\n]+")
                        (emacs-lisp-mode . "[^ \r\t\n]+")))))

;;; packages.el ends here

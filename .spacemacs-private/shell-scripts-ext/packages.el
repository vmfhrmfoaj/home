;;; packages.el --- shell-scripts-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst shell-scripts-ext-packages
  '((sh-script :location built-in)))

(defun shell-scripts-ext/post-init-sh-script ()
  (use-package sh-script
    :defer t
    :config
    (setq sh-basic-offset 2
          sh-indentation 2
          sh-learn-basic-offset t)
    (font-lock-add-keywords
     'sh-mode
     '(("\\(\\$[_0-9a-zA-Z]+\\)"
        (1 (let ((face (plist-get (text-properties-at (match-beginning 0)) 'face)) face-lst)
             (setq face-lst (if (listp face) face (list face)))
             (if (or (memq 'font-lock-comment-face face-lst)
                     (memq 'font-lock-string-face  face-lst))
                 face
               'default))
           t))
       ("\\(\\${[_0-9a-zA-Z]}+\\)"
        (1 'default t))
       ("\\([_0-9a-zA-Z]+\\)=[^~]"
        (1 'font-lock-variable-name-face))
       ("export\\s-+\\([_0-9a-zA-Z]+\\)"
        (1 'font-lock-variable-name-face)))
     'append)))

;;; packages.el ends here

;;; packages.el --- org-ext layer packages file for Spacemacs.
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

(defconst org-ext-packages
  '(
    org
    ))

(defun org-ext/post-init-org ()
  (use-package org
    :config
    (setq org-hide-emphasis-markers t
          org-pretty-entities t
          org-src-fontify-natively t
          org-startup-indented t
          org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌"))
    (font-lock-add-keywords
     'org-mode
     '(("^\\s-*\\(-\\) "
        1 (compose-region (match-beginning 1) (match-end 1) ?∙))
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)) t)
    (dolist (i (number-sequence 1 org-n-level-faces))
      (set-face-attribute (intern (concat "org-level-" (number-to-string i)))
                          nil
                          :weight 'bold))))

;;; packages.el ends here

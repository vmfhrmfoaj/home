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
          org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌")
          org-log-done 'time
          ;; NOTE:
          ;; references:
          ;; - http://pages.sachachua.com/.emacs.d/Sacha.html
          ;; - http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode
          ;; - http://orgmode.org/manual/Capture-templates.html#Capture-templates
          org-directory (concat (getenv "HOME") "/Desktop/Org")
          org-default-notes-file (concat org-directory "/todos/" (format-time-string "%Y") ".org")
          org-agenda-files (-> org-default-notes-file
                               (file-name-directory)
                               (directory-files)
                               (->> (-filter (-partial #'string-match-p "\\.org$"))
                                    (--map (concat org-directory "/todos/" it))
                                    (-filter #'file-exists-p)))
          org-capture-templates
          `(("t" "Todo" entry
             (file+headline ,org-default-notes-file ,(format-time-string "%b"))
             ,(concat "* TODO %^{Task}"                             "\n"
                      ":PROPERTIES:"                                "\n"
                      ":Effort: %^{Effort|1:00|3:00|6:00|1d|3d|6d}" "\n"
                      ":END:"                                       "\n"
                      "SCHEDULED: %t DEADLINE: %^t"                 "\n\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("n" "Note" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             "* %U %?"
             :empty-lines 1
             :prepend t)
            ("j" "Journal" entry
             (file+datetree ,(concat org-directory "/journal.org")))
            ("J" "Journal with date" entry
             (file+datetree+prompt ,(concat org-directory "/journal.org")))))
    (font-lock-add-keywords
     'org-mode
     '(("^\\s-*\\(-\\) "
        1 (compose-region (match-beginning 1) (match-end 1) ?∙))
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)) t)
    (dolist (i (number-sequence 1 org-n-level-faces))
      (set-face-attribute (intern (concat "org-level-" (number-to-string i)))
                          nil
                          :weight 'bold))
    (spacemacs/set-leader-keys
      "aoc" nil
      "aoct" #'org-capture-todo
      "aocT" #'org-capture-todo-done
      "aocn" #'org-capture-note
      "aocj" #'org-capture-journal
      "aocJ" #'org-capture-journal-with-prompt)
    (defun org-capture-todo ()
      (interactive)
      (org-capture nil "t"))
    (defun org-capture-todo-done ()
      (interactive)
      ;; TODO
      )
    (defun org-capture-note ()
      (interactive)
      (org-capture nil "n"))
    (defun org-capture-journal ()
      (interactive)
      (org-capture nil "j"))
    (defun org-capture-journal-with-prompt ()
      (interactive)
      (org-capture nil "J"))
    (advice-add #'org-capture :before
                (lambda (&rest _)
                  "Remove a duplicates history."
                  (->> (all-completions "org-capture-template-prompt-history" obarray)
                       (--map (intern it))
                       (--map (set it (-distinct (symbol-value it)))))))))

;;; packages.el ends here

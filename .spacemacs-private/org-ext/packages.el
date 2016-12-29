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
  '(org
    org-agenda
    (org-capture :location built-in)))

(defun org-ext/post-init-org ()
  (use-package org
    :defer t
    :config
    (setq org-directory (concat (getenv "HOME") "/Desktop/Org")
          org-hide-emphasis-markers t
          org-tags-column -90
          org-pretty-entities t
          org-src-fontify-natively t
          org-startup-indented t
          org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌")
          org-log-done 'time
          org-log-into-drawer t
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
          org-todo-keyword-faces '(("TODO" . org-todo)
                                   ("NEXT" . org-next)
                                   ("DONE" . org-done)
                                   ("CANCELLED" . org-cancelled)))
    (font-lock-add-keywords
     'org-mode
     '(("^\\s-*\\(-\\) "
        1 (compose-region (match-beginning 1) (match-end 1) ?·))
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)))
    (add-hook 'org-todo-get-default-hook
              (lambda (mark _)
                (when (string-equal mark "NEXT")
                  (org-insert-schedule-&-deadline)
                  nil)))))

(defun org-ext/post-init-org-agenda ()
  (use-package org-agenda
    :defer t
    :config
    (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                      (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                      (0.0 . '(:height 1.0)))
          org-agenda-files (find-org-agenda-files)
          org-agenda-skip-deadline-if-done t
          org-agenda-tags-column -95
          org-agenda-window-setup 'current-window)
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      (kbd "C-j") #'org-agenda-next-item
      (kbd "C-k") #'org-agenda-previous-item)))

(defun org-ext/init-org-capture ()
  (use-package org-capture
    :config
    (setq org-capture-templates
          `(("t" "Todo" entry
             (file+headline ,(concat org-directory "/todos/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* TODO %^{Task}"                                                  "\n"
                      ":PROPERTIES:"                                                     "\n"
                      ":Effort: %^{Effort|1:00|3:00|6:00|1d|3d|1w|2w|3w|1m|3m|6m|9m|1y}" "\n"
                      ":END:"                                                            "\n"
                      "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("n" "Note" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}" "\n"
                      "\n"
                      "%t:" "\n"
                      "- %?")
             :empty-lines 1
             :prepend t)
            ("p" "Protocol" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}"      "\n"
                      "\n"
                      "%t:"             "\n"
                      "- %c"            "\n"
                      "  #+BEGIN_QUOTE" "\n"
                      "  %i"            "\n"
                      "  #+End_QUOTE"   "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("L" "Protocol Link" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}" "\n"
                      "\n"
                      "%t:"  "\n"
                      "- %c" "\n"
                      "%?")
             :empty-lines 1
             :prepend t)))
    (spacemacs/set-leader-keys
      "aoc" nil
      "aoct" (defalias 'org-capture-todo   (lambda () (interactive) (org-capture nil "t")))
      "aocn" (defalias 'org-capture-note   (lambda () (interactive) (org-capture nil "n"))))
    (advice-add #'org-set-tags :after #'remove-duplicated-org-tags-history)))

;;; packages.el ends here

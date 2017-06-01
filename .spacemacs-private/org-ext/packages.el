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
    evil-org))

(defun org-ext/post-init-org ()
  (use-package org
    :defer t
    :config
    (setq org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌")
          org-complete-tags-always-offer-all-agenda-tags t
          org-directory (concat (getenv "HOME") "/Desktop/Org")
          org-hide-emphasis-markers t
          org-insert-schedule-deadline t
          org-log-done 'time
          org-log-into-drawer t
          org-pretty-entities t
          org-src-fontify-natively t
          org-startup-indented t
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
                (when (and org-insert-schedule-deadline
                           (string-equal mark "NEXT"))
                  (org-insert-schedule-&-deadline)
                  nil)))
    (add-hook 'org-after-todo-statistics-hook
              (lambda (num-done-task num-remaining-task)
                (let (org-log-done
                      org-log-states
                      org-insert-schedule-deadline)
                  (cond
                   ((= 0 num-remaining-task) (org-todo "DONE"))
                   ((= 0 num-done-task)      (org-todo "TODO"))
                   (t))))))

  (use-package org-agenda
    :defer t
    :config
    (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                      (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                      (0.0 . '(:height 1.0)))
          org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
          org-agenda-files (find-org-agenda-files)
          org-agenda-skip-deadline-if-done t
          org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                        (todo   todo-state-down priority-down category-keep)
                                        (tags   priority-down category-keep)
                                        (search category-keep))
          org-agenda-tags-column org-tags-column
          org-agenda-window-setup 'current-window)
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      (kbd "C-j") #'org-agenda-next-item
      (kbd "C-k") #'org-agenda-previous-item))

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
                      "- %a"            "\n"
                      "  #+BEGIN_QUOTE" "\n"
                      "  %i"            "\n"
                      "  #+END_QUOTE"   "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("L" "Protocol Link" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}" "\n"
                      "\n"
                      "%t:"  "\n"
                      "- %a" "\n"
                      "%?")
             :empty-lines 1
             :prepend t)))
    (spacemacs/set-leader-keys
      "aoc" nil
      "aoct" (defalias 'org-capture-todo (lambda () (interactive) (org-capture nil "t")))
      "aocn" (defalias 'org-capture-note (lambda () (interactive) (org-capture nil "n")))
      "aogc" #'org-clock-jump-to-current-clock))

  (use-package org-protocol
    :config
    (advice-add #'org-protocol-sanitize-uri :filter-return
                (lambda (url)
                  (if org-capture-use-cached-url
                      (concat "http://webcache.googleusercontent.com/search?q=cache:"
                              (url-hexify-string url))
                    url))))

  (use-package org-clock
    :defer t
    :config
    (setq org-clock-into-drawer t)
    (advice-add #'org-clock-get-clocktable :filter-return
                (lambda (tlb)
                  (concat "\n" (propertize "CLOCKING:" 'face 'org-agenda-date)
                          "\n" (->> (split-string tlb "\n")
                                    (--map (concat "  " it))
                                    (-interpose "\n")
                                    (apply #'concat))))))
  (use-package org-colview
    :defer t
    :config
    (setq org-columns-default-format "%40ITEM %TODO %5Effort %5CLOCKSUM %TAGS")))

(defun org-ext/post-init-evil-org ()
  (use-package evil-org
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "C" #'org-columns)))

;;; packages.el ends here

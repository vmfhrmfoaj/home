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
  '(evil-org
    org
    org-agenda))

(defun org-ext/post-init-evil-org ()
  (use-package evil-org
    :ensure t
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "C" #'org-columns)))

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
                                   ("CANCELLED" . org-cancelled))
          org-use-sub-superscripts nil)
    (spacemacs/declare-prefix-for-mode 'org-mode "mu" "update")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ":" #'org-set-tags
      "it" (defalias 'org-isnert-task-item
             (byte-compile
              (lambda ()
                "Insert a new task at current level."
                (interactive)
                (-if-let (pos (org-in-item-p))
                    (progn
                      (goto-char pos)
                      (re-search-forward "- " (line-end-position)))
                  (insert "- "))
                (if (looking-at "\\[[\\( \\|-\\)]\\] ")
                    (goto-char (match-end 0))
                  (insert "[ ] ")
                  (org-update-checkbox-count-maybe)))))
      "ul" #'org-url=>cached-url)
    (advice-add #'org-insert-item :filter-args
                (byte-compile
                 (lambda (checkbox)
                   "Imporve the `clever-insert-item'."
                   (let ((pos (org-in-item-p)))
                     (if (and checkbox
                              (not pos))
                         checkbox
                       (save-excursion
                         (goto-char pos)
                         (list (looking-at-p "[ ]*- \\[\\( \\|-\\|X\\)\\]"))))))))
    (font-lock-add-keywords
     'org-mode
     '(("^\\s-*\\(-\\) "
        1 (progn
            (let ((s (match-beginning 1))
                  (e (match-end 1)))
              (compose-region s e ?·))
            'bold))
       ("^\\s-*\\(\\([0-9]\\.\\)\\) "
        1 'bold)
       ("^\\s-*\\(?:-\\|[0-9]+\\.\\) \\(\\[\\( \\|-\\|X\\)\\]\\) "
        1 (progn
            (let ((x (match-string 2))
                  (s (match-beginning 1))
                  (e (match-end 1)))
              (compose-region
               s e
               (->> (all-the-icons-faicon-data)
                    (assoc (cond
                            ((string-equal x " ") "square")
                            ((string-equal x "-") "minus-square")
                            ((string-equal x "X") "check-square")))
                    (cdr)
                    (string-to-char)))
              (put-text-property s e 'display '(raise -0.2))
              (list (list :family "FontAwesome"
                          :foreground (face-attribute (if (string-equal x "X")
                                                          'org-done 'org-todo)
                                                      :foreground))))))
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)))
    (add-hook 'org-todo-get-default-hook
              (byte-compile
               (lambda (mark _)
                 "Set a schedule and deadline for NEXT."
                 (when (and org-insert-schedule-deadline
                            (string-equal mark "NEXT"))
                   (org-insert-schedule-&-deadline)
                   nil))))
    (add-hook 'org-after-todo-statistics-hook
              (byte-compile
               (lambda (num-done-task num-remaining-task)
                 "Automatically changes the status of _TODO_ according to sub-TODO."
                 (let (org-log-done
                       org-log-states
                       org-insert-schedule-deadline)
                   (cond
                    ((= 0 num-remaining-task) (org-todo "DONE"))
                    ((= 0 num-done-task)      (org-todo "TODO"))
                    (t))))))
    (advice-add #'org-todo :around
                (byte-compile
                 (lambda (of &optional arg)
                   "If reopen the completed _TODO_, show a popup for logging."
                   (let* ((is-done? (member (org-get-todo-state) org-done-keywords))
                          (org-todo-log-states (if is-done?
                                                   (append '(("TODO" note time)
                                                             ("NEXT" note time))
                                                           org-todo-log-states)
                                                 org-todo-log-states)))
                     (funcall of arg)))))
    (add-hook 'org-metaleft-hook
              (byte-compile
               (lambda ()
                 (unless (org-at-heading-or-item-p)
                   (call-interactively #'evil-shift-left)
                   t))))
    (add-hook 'org-metaright-hook
              (byte-compile
               (lambda ()
                 (unless (org-at-heading-or-item-p)
                   (call-interactively #'evil-shift-right)
                   t)))))

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
             :prepend t)
            ("n" "Note" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}" "\n"
                      "\n"
                      "%t:" "\n"
                      "- %?")
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
             :prepend t)
            ("L" "Protocol Link" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}" "\n"
                      "\n"
                      "%t:"  "\n"
                      "- %a" "\n"
                      "%?")
             :prepend t)))
    (spacemacs/set-leader-keys
      "aoc" nil
      "aoct" (defalias 'org-capture-todo (lambda () (interactive) (org-capture nil "t")))
      "aocn" (defalias 'org-capture-note (lambda () (interactive) (org-capture nil "n")))
      "aogc" #'org-clock-jump-to-current-clock))

  (use-package org-protocol
    :config
    (byte-compile #'org-protocol-cached-url)
    (advice-add #'org-protocol-sanitize-uri :filter-return
                (byte-compile
                 (lambda (url)
                   (if org-capture-use-cached-url
                       (org-protocol-cached-url url)
                     url)))))

  (use-package org-clock
    :defer t
    :config
    (setq org-clock-into-drawer t)
    (advice-add #'org-clock-get-clocktable :filter-return
                (byte-compile
                 (lambda (tlb)
                   "Pretty the org clock table."
                   (concat "\n" (propertize "CLOCKING:" 'face 'org-agenda-date)
                           "\n" (->> (split-string tlb "\n")
                                     (--map (concat "  " it))
                                     (-interpose "\n")
                                     (apply #'concat)))))))
  (use-package org-colview
    :defer t
    :config
    (setq org-columns-default-format "%40ITEM %TODO %5Effort %7CLOCKSUM %TAGS")))

(defun org-ext/post-init-org-agenda ()
  (use-package org-agenda
    :defer t
    :config
    (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                      (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                      (0.0 . '(:height 1.0)))
          org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
          org-agenda-files (org-agenda-find-files)
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                        (todo   todo-state-down priority-down category-keep)
                                        (tags   priority-down category-keep)
                                        (search category-keep))
          org-agenda-tags-column org-tags-column
          org-agenda-window-setup 'current-window)
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      (kbd "h") #'org-agenda-earlier
      (kbd "j") #'org-agenda-next-item
      (kbd "k") #'org-agenda-previous-item
      (kbd "l") #'org-agenda-later
      (kbd "D") #'org-agenda-day-view
      (kbd "W") #'org-agenda-week-view
      (kbd "M") #'org-agenda-month-view
      (kbd "Y") #'org-agenda-year-view)
    (with-eval-after-load "persp-mode"
      (spacemacs|define-custom-layout "@Org"
        :binding "o"
        :body
        (->> (buffer-list)
             (--filter (-when-let (file-name (buffer-file-name it))
                         (member file-name org-agenda-files)))
             (-map #'persp-add-buffer))))))

;;; packages.el ends here

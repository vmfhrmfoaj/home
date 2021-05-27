;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'evil-org nil t)
  (require 'org nil t)
  (require 'org-agenda nil t)
  (require 'org-capture nil t)
  (require 'org-clock nil t))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode))

(use-package org
  :defer t
  :init
  (defface org-next
    '((t (:inherit org-todo)))
    "TODO")

  (defface org-cancelled
    '((t (:inherit org-done)))
    "TODO")

  (setq org-directory (concat (getenv "HOME") "/Desktop/Org"))

  :config
  (defun org-insert-schedule-&-deadline (mark &optional _)
    "Set a schedule and deadline for NEXT."
    (when org-insert-schedule-deadline
      (cond
       ((or (string-equal mark "TODO")
            (string-equal mark "PLANNING"))
        (org-remove-timestamp-with-keyword "CLOSED:")
        (org-remove-timestamp-with-keyword "DEADLINE:")
        (org-remove-timestamp-with-keyword "SCHEDULED:"))
       ((string-equal mark "NEXT")
        (org-schedule 'overwrite "+0d")
        (org-deadline 'overwrite)))
      nil))

  (defun org-tags-completion-function-for-case-insensitive (string _predicate &optional flag)
    "Complete tag STRING.
FLAG specifies the type of completion operation to perform.  This
function is passed as a collection function to `completing-read',
which see."
    (let ((completion-ignore-case t)	;tags are case-sensitive
	      (confirm (lambda (x) (stringp (car x))))
	      (prefix ""))
      (when (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
        (setq prefix (match-string 1 string))
        (setq string (match-string 2 string)))
      (pcase flag
        (`t (all-completions string org-last-tags-completion-table confirm))
        (`lambda (assoc string org-last-tags-completion-table)) ;exact match?
        (`nil
         (pcase (try-completion string org-last-tags-completion-table confirm)
	       ((and completion (pred stringp))
	        (concat prefix
		            completion
		            (if (and org-add-colon-after-tag-completion
			                 (assoc completion org-last-tags-completion-table))
		                ":"
		              "")))
	       (completion completion)))
        (_ nil))))

  (setq org-complete-tags-always-offer-all-agenda-tags t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-fontify-todo-headline nil ; Add custom `font-lock' rules
        org-fontify-done-headline nil
        org-hide-emphasis-markers t
        org-image-actual-width (* 100 (frame-char-width))
        org-insert-schedule-deadline t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-src-fontify-natively t
        org-startup-indented t
        org-adapt-indentation nil
        org-startup-with-inline-images t
        org-todo-keywords '((sequence "TODO(t)" "PLANNING" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("TODO"      . org-todo)
                                 ("PLANNING"  . org-next)
                                 ("NEXT"      . org-next)
                                 ("DONE"      . org-done)
                                 ("CANCELLED" . org-cancelled))
        org-use-sub-superscripts nil
        org-tags-column -120)

  ;; NOTE
  ;;  To prevnt multi-line `org-emphasis'.
  (setf (nth 3 org-emphasis-regexp-components) ".*?")
  (setf (nth 4 org-emphasis-regexp-components) 0)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'org-todo-get-default-hook #'org-insert-schedule-&-deadline)
  (add-hook 'org-mode-hook
            (lambda ()
              ;; NOTE
              ;;  disable `show-smartparens-mode' due to it is conflicted with `org-indent-mode'.
              ;;  `show-paren-mode' is also conflicted with it.
              (turn-off-show-smartparens-mode))
            100)

  (advice-add #'org-tags-completion-function :override #'org-tags-completion-function-for-case-insensitive)
  (advice-add #'org-todo :around
              (lambda (of &optional arg)
                "If reopen the completed _TODO_, show a popup for logging."
                (cl-letf (((symbol-function 'delete-other-windows) #'ignore))
                  (let* ((is-done? (member (org-get-todo-state) org-done-keywords))
                         (org-todo-log-states (if is-done?
                                                  (append '(("TODO" note time)
                                                            ("NEXT" note time))
                                                          org-todo-log-states)
                                                org-todo-log-states)))
                    (funcall of arg))))))

(use-package org-agenda
  :defer t
  :init
  (setq calendar-holidays nil
        org-agenda-files
        (when (file-exists-p org-directory)
          (directory-files-recursively org-directory "\\.org$")))

  :config
  (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0))
                                    (0.5 . '(:inherit org-upcoming-deadline :height 1.0))
                                    (0.0 . '(:height 1.0)))
        org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
        org-agenda-scheduled-leaders '("Scheduled: " "Sched.%03dx: ")
        org-agenda-deadline-leaders  '("Deadline:  " "In %03d d.: " "%02d d. ago: ")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-function-global
        (lambda ()
          (and (string-match-p "(a)" org-agenda-buffer-name)
               (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
               (org-agenda-skip-subtree-if 'scheduled)
               (org-agenda-skip-subtree-if 'notdeadline)))
        org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo   todo-state-down priority-down category-keep)
                                      (tags   priority-down category-keep)
                                      (search category-keep))
        org-agenda-sticky t
        ;; org-agenda-tags-column org-tags-column
        org-agenda-window-setup 'current-window
        org-agenda-prefix-format '((agenda . " ")
                                   (todo   . " ")
                                   (tags   . " ")
                                   (search . " ")))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq-local default-directory (concat home-dir "/Desktop/Org/")
                          projectile-project-name "Org")
              (let* ((filter "Work")
                     (cur-time (current-time))
                     (cur-date (-drop 3 (decode-time cur-time)))
                     (10am (->> cur-date (-concat '(0 0 10)) (encode-time)))
                     (7pm  (->> cur-date (-concat '(0 0 19)) (encode-time))))
                (setq filter (concat (if (or (when (require 'diary-lib nil t)
                                               (-let (((d m y) cur-date))
                                                 (not (null (diary-list-entries (list m d y) 1 t)))))
                                             (memq (calendar-day-of-week (calendar-current-date)) '(0 6))
                                             (time-less-p cur-time 10am)
                                             (not (time-less-p cur-time 7pm)))
                                         "-"
                                       "+")
                                     filter))
                (add-to-list 'org-agenda-tag-filter filter)))))

(use-package org-capture
  :defer t
  :commands (org-capture)
  :init
  (defun org-capture-todo ()
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-note ()
    (interactive)
    (org-capture nil "n"))

  :config
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(concat org-directory "/todos/" (format-time-string "%Y") ".org")
                          ,(format-time-string "%b"))
           ,(concat "* TODO %^{Task}"                                                    "\n"
                    ":PROPERTIES:"                                                       "\n"
                    ":Effort:   %^{Effort|1:00|3:00|6:00|1d|3d|1w|2w|3w|1m|3m|6m|9m|1y}" "\n"
                    ":END:"                                                              "\n"
                    ""                                                                   "\n"
                    "%?"                                                                 "\n"
                    ""                                                                   "\n"
                    "_CHECKLIST_:"                                                       "\n"
                    ""                                                                   "\n"
                    "_BACKLOG_:"                                                         "\n")
           :prepend t
           :empty-lines-after 1)))

  (advice-add #'org-capture-place-template :around
              (lambda (fn &rest args)
                "To prevent delete windows"
                (cl-letf (((symbol-function 'delete-other-windows) #'ignore))
                  (apply fn args)))))

(use-package org-clock
  :defer t
  :commands (org-clock-jump-to-current-clock)
  :config
  (defun org-clock--resume ()
    (when (frame-focus-state)
      (remove-function after-focus-change-function #'org-clock--resume)
      (org-clock-in-last)))

  (defun org-clock--auto-stop-&-restart ()
    (when (org-clocking-p)
      (let* ((idle-time (seconds-to-time (* 60 org-clock-custom-idle-time)))
             (at-time (time-subtract (current-time) idle-time)))
        (org-clock-out nil t at-time)
        (add-function :after after-focus-change-function #'org-clock--resume))))

  (defun org-clock--stop ()
    (when (org-clocking-p)
      (org-clock-goto)
      (org-clock-out nil t)
      (save-buffer)))

  (defun org-clock--start-idle-timer ()
    (add-hook 'kill-emacs-hook #'org-clock--stop)
    (run-with-idle-timer (* 60 org-clock-custom-idle-time) t
                         #'org-clock--auto-stop-&-restart))

  (setq org-clock-into-drawer t
        org-clock-idle-time nil
        org-clock-custom-idle-time 30
        org-clock-custom-idle-timer (org-clock--start-idle-timer))
  (advice-add #'org-clock--mode-line-heading :override (-const "CLOCK-IN")))

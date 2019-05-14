(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-set-key-theme))

(use-package evil-org-agenda
  :after org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package org
  :ensure org-plus-contrib
  :defer t
  :init
  (defface org-next
    '((t (:inherit org-todo)))
    "TODO")

  (defface org-cancelled
    '((t (:inherit org-done)))
    "TODO")

  (defn org-insert-schedule-&-deadline (mark &optional _)
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

  (defn org-tags-completion-function-for-case-insensitive (string _predicate &optional flag)
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

  (defn org-dic-at-point ()
    "TODO"
    (interactive)
    (when (fboundp #'osx-dictionary-search-pointer)
      (osx-dictionary-search-pointer)))

  (setq org-directory (concat (getenv "HOME") "/Desktop/Org"))

  :config
  (require 'helm-org nil t)
  (setq org-complete-tags-always-offer-all-agenda-tags t
        org-duration-units (let* ((ava-hours-in-day 12)
                                  (d (- (* ava-hours-in-day 60) ; pomodoro
                                        (* ava-hours-in-day 10)
                                        (* (/ ava-hours-in-day 2) 10))))
                            `(("min" . 1)
                              ("h"   . 60)
                              ("d"   . ,d)
                              ("w"   . ,(* d 7))
                              ("m"   . ,(* d 30))
                              ("y"   . ,(* d 24 365.25))))
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-image-actual-width nil
        org-insert-schedule-deadline t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-src-fontify-natively t
        org-startup-indented t
        org-startup-with-inline-images t
        org-todo-keywords '((sequence "TODO(t)" "PLANNING" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("TODO"      . org-todo)
                                 ("PLANNING"  . org-next)
                                 ("NEXT"      . org-next)
                                 ("DONE"      . org-done)
                                 ("CANCELLED" . org-cancelled))
        org-use-sub-superscripts nil
        org-use-fast-tag-selection nil)
  (-when-let (it (assoc 'file org-link-frame-setup))
    (setf (cdr it) #'find-file))
  (with-eval-after-load "helm-mode"
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-set-tags-command . helm-org-completing-read-tags)))
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'org-todo-get-default-hook #'org-insert-schedule-&-deadline)
  (add-hook 'org-mode-hook
            (lambda ()
              "TODO"
              (setq-local evil-lookup-func #'org-dic-at-point)
              (when (and (eq (persp-current-name) persp-org-name)
                         (buffer-file-name)
                         (->> (file-relative-name buffer-file-name org-directory)
                              (string-match-p "\\.\\./")
                              (not)))
                (setq-local projectile-indexing-method 'native))))
  (advice-add #'org-tags-completion-function :override #'org-tags-completion-function-for-case-insensitive)
  (advice-add #'org-clock-goto :before (byte-compile (lambda (&optional select) (persp-switch-to-org))))
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
                   (funcall of arg))))))

(use-package org-agenda
  :ensure org-plus-contrib
  :defer t
  :init
  (defn org-agenda-resume ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (when (and (bufferp org-agenda-buffer)
               (buffer-live-p org-agenda-buffer))
      (switch-to-buffer org-agenda-buffer)
      (call-interactively #'org-agenda-redo)
      t))

  (defn org-agenda-show-list ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-agenda-list)
    (call-interactively #'org-agenda-redo))

  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  :config
  (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                    (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                    (0.0 . '(:height 1.0)))
        org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
        org-agenda-scheduled-leaders '("Scheduled: " "Sched.%03dx: ")
        org-agenda-deadline-leaders  '("Deadline:  " "In %03d d.: " "%02d d. ago: ")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
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
        org-agenda-tags-column org-tags-column
        org-agenda-window-setup 'current-window
        org-babel-load-languages '((R . t)
                                   (emacs-lisp . t)
                                   (gnuplot . t)
                                   (ledger . t)))
  (with-eval-after-load "persp-mode"
    (let ((f (byte-compile
              (lambda (&rest _)
                (persp-switch-to-org)))))
      (dolist (target-fn '(org-clock-jump-to-current-clock
                           org-search-view
                           org-tags-view
                           org-todo-list))
        (advice-add target-fn :before f)))
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (when (string-equal persp-org-name (persp-current-name))
                  (persp-add-buffer-without-switch))
                (when (eq (persp-current-name) persp-org-name)
                  (setq-local projectile-indexing-method 'native))))))

(use-package org-capture
  :defer t
  :init
  (defn org-capture-todo ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-capture nil "t"))

  (defn org-capture-note ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-capture nil "n"))

  :config
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(concat org-directory  "/" (format-time-string "%Y") "/todo.org")
                          ,(format-time-string "%b"))
           ,(concat "* TODO %^{Task}"                                                    "\n"
                    ":PROPERTIES:"                                                       "\n"
                    ":Effort:   %^{Effort|1:00|3:00|6:00|1d|3d|1w|2w|3w|1m|3m|6m|9m|1y}" "\n"
                    ":END:"                                                              "\n"
                    "\n"
                    "%?"
                    "\n")
           :prepend t)
          ("n" "Note" entry
           (file+headline ,(concat org-directory  "/" (format-time-string "%Y") "/note.org")
                          ,(format-time-string "%b"))
           ,(concat "* %^{Note}" "\n"
                    "\n"
                    "- %?"
                    "\n")
           :prepend t))))

(use-package org-clock
  :defer t
  :commands (org-clock-jump-to-current-clock)

  :init
  (defn org-clock--resume ()
    "TODO"
    (remove-hook 'focus-in-hook 'org-clock--resume)
    (org-clock-in-last))

  (defn org-clock--auto-stop-&-restart ()
    "TODO"
    (when (org-clocking-p)
      (let* ((idle-time (seconds-to-time (* 60 org-clock-custom-idle-time)))
             (at-time (time-subtract (current-time) idle-time)))
        (org-clock-out nil t at-time)
        (add-hook 'focus-in-hook #'org-clock--resume))))

  (defn org-clock--stop ()
    "TODO"
    (when (org-clocking-p)
      (org-clock-goto)
      (org-clock-out nil t)
      (save-buffer)))

  (defn org-clock--start-idle-timer ()
    "TODO"
    (add-hook 'kill-emacs-hook #'org-clock--stop)
    (run-with-idle-timer (* 60 org-clock-custom-idle-time) t
                         #'org-clock--auto-stop-&-restart))

  :config
  (setq org-clock-into-drawer t
        org-clock-idle-time nil
        org-clock-custom-idle-time 30
        org-clock-custom-idle-timer (org-clock--start-idle-timer))
  (advice-add #'org-clock--mode-line-heading :override (-const "CLOCK-IN")))

(use-package org-protocol
  :ensure org-plus-contrib
  :defer t
  :init
  (defn org-protocol-setup ()
    "TODO"
    (remove-hook 'focus-out-hook #'org-protocol-setup)
    (require 'org-capture)
    (require 'org-protocol))

  (add-hook 'focus-out-hook #'org-protocol-setup))

(use-package org-trello
  :ensure t
  :defer t
  :init
  (setq org-trello-files (-> org-directory
                             (concat "/trello")
                             (directory-files-recursively "\\.org$"))
        org-trello-input-completion-mechanism 'helm)
  (add-hook 'org-mode-hook
            (lambda ()
              (when (->> org-trello-files (--any? (string= (expand-file-name it) buffer-file-name)))
                (org-trello-mode))))

  :config
  (require 'org-trello-setup nil t)
  (setq org-trello-buffer--indent-description 0))

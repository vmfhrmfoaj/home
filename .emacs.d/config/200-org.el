(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

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

  (defun org-dic-at-point ()
    (interactive)
    (when (fboundp #'osx-dictionary-search-pointer)
      (osx-dictionary-search-pointer)))

  (setq org-complete-tags-always-offer-all-agenda-tags t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
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
              (setq-local evil-lookup-func #'org-dic-at-point)
              ;; NOTE
              ;;  disable `show-smartparens-mode' due to it is conflicted with `org-indent-mode'.
              ;;  `show-paren-mode' is also conflicted with it.
              (turn-off-show-smartparens-mode))
            100)

  (advice-add #'org-tags-completion-function :override #'org-tags-completion-function-for-case-insensitive)
  (advice-add #'org-clock-goto :before (lambda (&optional select) (persp-switch-to-org)))
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
  :init
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  (defun org-agenda-show-on-dedicated-window (org-agenda-fn &optional finish-fn)
    (if-let ((win (-some->> (window-list)
                            (--filter (with-current-buffer (window-buffer it)
                                        (derived-mode-p 'org-mode 'org-agenda-mode)))
                            (-first-item))))
        (unwind-protect
            (progn
              (set-window-dedicated-p win nil)
              (select-window win)
              (funcall org-agenda-fn))
          (set-window-dedicated-p win t))
      (funcall org-agenda-fn))
    (call-interactively #'org-agenda-redo)
    (setq-local default-directory (concat home-dir "/Desktop/Org/")
                frame--width (frame-width)
                projectile-project-name "Org")
    (when finish-fn
      (funcall finish-fn))
    (when (fboundp #'golden-ratio)
      (golden-ratio)))

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

  (add-hook 'window-size-change-functions
            (lambda (&rest _)
              "Align tags"
              (when-let ((win (and org-agenda-buffer
                                   (get-buffer-window org-agenda-buffer))))
                (with-selected-window win
                  (let ((w (window-width)))
                    (when (and (boundp 'org-agenda--width)
                               (not (= org-agenda--width w)))
                      (org-agenda-align-tags))
                    (setq-local org-agenda--width w))))))

  (advice-add #'org-add-log-note :override
              (lambda (&optional _purpose)
                "Fix for the dedicated window"
                (remove-hook 'post-command-hook 'org-add-log-note)
                (setq org-log-note-window-configuration (current-window-configuration))
                (move-marker org-log-note-return-to (point))
                (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
                (goto-char org-log-note-marker)
                (org-switch-to-buffer-other-window (get-buffer-create "*Org Note*"))
                (erase-buffer)
                (if (memq org-log-note-how '(time state))
                    (org-store-log-note)
                  (let ((org-inhibit-startup t)) (org-mode))
                  (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		                          (cl-case org-log-note-purpose
		                            (clock-out "stopped clock")
		                            (done  "closed todo item")
		                            (reschedule "rescheduling")
		                            (delschedule "no longer scheduled")
		                            (redeadline "changing deadline")
		                            (deldeadline "removing deadline")
		                            (refile "refiling")
		                            (note "this entry")
		                            (state
		                             (format "state change from \"%s\" to \"%s\""
			                                 (or org-log-note-previous-state "")
			                                 (or org-log-note-state "")))
		                            (t (error "This should not happen")))))
                  (when org-log-note-extra (insert org-log-note-extra))
                  (setq-local org-finish-function 'org-store-log-note)
                  (run-hooks 'org-log-buffer-setup-hook)))))

(use-package org-capture
  :defer t
  :config
  (defun org-capture-todo ()
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-capture nil "t"))

  (defun org-capture-note ()
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-capture nil "n"))

  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(concat org-directory "/todos/" (format-time-string "%Y") ".org")
                          ,(format-time-string "%b"))
           ,(concat "* TODO %^{Task}"                                                    "\n"
                    ":PROPERTIES:"                                                       "\n"
                    ":Effort:   %^{Effort|1:00|3:00|6:00|1d|3d|1w|2w|3w|1m|3m|6m|9m|1y}" "\n"
                    ":END:"                                                              "\n"
                    "\n"
                    "_REQUIREMENTS_:"                                                    "\n"
                    "- %?"                                                               "\n"
                    "\n"
                    "_CHECKLIST_:"                                                       "\n"
                    "- [ ] ???"                                                          "\n"
                    "\n"
                    "_BACKLOG_:"                                                         "\n"
                    "- [ ] ???"                                                          "\n"
                    "\n")
           :prepend t)))

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

(use-package org-protocol
  :defer t
  :init
  (defun org-protocol-setup ()
    (unless (frame-focus-state)
      (remove-function after-focus-change-function #'org-protocol-setup)
      (require 'org-capture)
      (require 'org-protocol)))

  (if window-system
      ;; NOTE
      ;;  I don't know why `after-focus-change-function' was triggered when starting Emacs.
      ;;  To workaround to adds the setup function to the focus event hook with 0.5 seconds after starting Emacs.
      (add-hook 'after-init-hook
                (lambda ()
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (add-function :after after-focus-change-function #'org-protocol-setup)))))
    (require 'org-capture)
    (require 'org-protocol)))

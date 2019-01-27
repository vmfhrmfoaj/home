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
    "TODO"
    (interactive)
    (when (fboundp #'osx-dictionary-search-pointer)
      (osx-dictionary-search-pointer)))

  (setq org-directory (concat (getenv "HOME") "/Desktop/Org"))

  :config
  (require 'helm-org nil t)
  (setq org-complete-tags-always-offer-all-agenda-tags t
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
        org-use-sub-superscripts nil)
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command . helm-org-completing-read-tags))
  (add-hook 'org-todo-get-default-hook #'org-insert-schedule-&-deadline)
  (add-hook 'org-mode-hook
            (lambda ()
              "TODO"
              (setq-local evil-lookup-func #'org-dic-at-point)))
  (advice-add #'org-tags-completion-function :override #'org-tags-completion-function-for-case-insensitive)
  (advice-add #'org-todo :around
              (lambda (of &optional arg)
                "If reopen the completed _TODO_, show a popup for logging."
                (let* ((is-done? (member (org-get-todo-state) org-done-keywords))
                       (org-todo-log-states (if is-done?
                                                (append '(("TODO" note time)
                                                          ("NEXT" note time))
                                                        org-todo-log-states)
                                              org-todo-log-states)))
                  (funcall of arg)))))

(use-package org-agenda
  :ensure org-plus-contrib
  :defer t
  :init
  (defun org-agenda-resume ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (when (and (bufferp org-agenda-buffer)
               (buffer-live-p org-agenda-buffer))
      (switch-to-buffer org-agenda-buffer)
      (call-interactively #'org-agenda-redo)
      t))

  (defun org-agenda-show-list ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-agenda-list)
    (call-interactively #'org-agenda-redo))

  (defun org-sync-cloud ()
    "TODO"
    (interactive)
    (let ((dir (concat (getenv "HOME") "/Google Drive/Org")))
      (message (concat "Updating " dir "..."))
      (pull-to-google-drive
       dir
       (-partial
        (lambda (cur-win _)
          (when (and (boundp 'org-agenda-buffer)
                     (buffer-live-p org-agenda-buffer))
            (flet ((yes-or-no-p (&rest args) t))
              (dolist (file org-agenda-files)
                (find-file-noselect file)))
            (with-selected-window cur-win
              (with-current-buffer org-agenda-buffer
                (call-interactively #'org-agenda-redo)))))
        (selected-window)))))

  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  :config
  (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                    (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                    (0.0 . '(:height 1.0)))
        org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
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
        org-agenda-window-setup 'current-window)
  (when window-system
    (org-sync-cloud))
  (with-eval-after-load "persp-mode"
    (let ((f (lambda (&rest _)
               (persp-switch-to-org))))
      (dolist (target-fn '(org-clock-jump-to-current-clock
                           org-search-view
                           org-tags-view
                           org-todo-list))
        (advice-add target-fn :before f)))
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (when (string-equal persp-org-name (persp-current-name))
                  (persp-add-buffer-without-switch))))))

(use-package org-capture
  :defer t
  :init
  (defun org-capture-todo ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
    (org-capture nil "t"))

  (defun org-capture-note ()
    "TODO"
    (interactive)
    (when (fboundp #'persp-switch-to-org)
      (persp-switch-to-org))
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
           :prepend t))))

(use-package org-clock
  :defer t
  :commands (org-clock-jump-to-current-clock)

  :init
  (defun org-clock--resume ()
    "TODO"
    (remove-hook 'focus-in-hook 'org-clock--resume)
    (org-clock-in-last))

  (defun org-clock--auto-stop-&-restart ()
    "TODO"
    (when (org-clocking-p)
      (let* ((idle-time (seconds-to-time (* 60 org-clock-custom-idle-time)))
             (at-time (time-subtract (current-time) idle-time)))
        (org-clock-out nil t at-time)
        (add-hook 'focus-in-hook #'org-clock--resume))))

  (defun org-clock--stop ()
    "TODO"
    (when (org-clocking-p)
      (org-clock-goto)
      (org-clock-out nil t)
      (save-buffer)))

  (defun org-clock--start-idle-timer ()
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
  (defun org-protocol-setup ()
    "TODO"
    (remove-hook 'focus-out-hook #'org-protocol-setup)
    (require 'org-capture)
    (require 'org-protocol))

  (add-hook 'focus-out-hook #'org-protocol-setup))

(use-package org-trello
  :ensure t
  :defer t
  :init
  (setq org-trello-current-prefix-keybinding "SPC m x")

  :config
  (setq org-trello-files (-> org-directory
                             (concat "/trello")
                             (directory-files-recursively "\\.org$"))
        org-trello-input-completion-mechanism 'helm))

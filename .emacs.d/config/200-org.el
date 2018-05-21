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

  :config
  (setq org-bullets-bullet-list '("■" "□" "◙" "◘" "●" "○" "◌")
        org-complete-tags-always-offer-all-agenda-tags t
        org-directory (concat (getenv "HOME") "/Desktop/Org")
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-image-actual-width nil
        org-insert-schedule-deadline t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-src-fontify-natively t
        org-startup-indented t
        org-startup-with-inline-images t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("TODO" . org-todo)
                                 ("NEXT" . org-next)
                                 ("DONE" . org-done)
                                 ("CANCELLED" . org-cancelled))
        org-use-sub-superscripts nil)

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
  :defer t
  :config
  (setq org-agenda-deadline-faces '((1.0 . '(:inherit org-warning :height 1.0 :weight bold))
                                    (0.5 . '(:inherit org-upcoming-deadline :height 1.0 :weight bold))
                                    (0.0 . '(:height 1.0)))
        org-agenda-clockreport-parameter-plist '(:link t :fileskip0 t :stepskip0 t :maxlevel 5 :tcolumns 1 :narrow 70!)
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-function-global
        (byte-compile
         (lambda ()
           (and (eq (car org-agenda-redo-command) 'org-agenda-list)
                (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                (org-agenda-skip-subtree-if 'scheduled)
                (org-agenda-skip-subtree-if 'notdeadline))))
        org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo   todo-state-down priority-down category-keep)
                                      (tags   priority-down category-keep)
                                      (search category-keep))
        org-agenda-tags-column org-tags-column
        org-agenda-window-setup 'current-window))

(use-package org-capture
  :defer t
  :init
  (defun org-capture-todo ()
    "TODO"
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-note ()
    "TODO"
    (interactive)
    (org-capture nil "n"))

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
           :prepend t))))

(use-package org-protocol
  :defer t
  :init
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'org-protocol))))

(use-package org-clock
  :defer t
  :commands (org-clock-in
             org-clock-out
             org-clock-jump-to-current-clock)

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
        org-clock-custom-idle-timer (org-clock--start-idle-timer)))

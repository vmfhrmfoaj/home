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
    (org-capture :location built-in)
    org-projectile
    (org-protocol :location built-in)
    projectile))

(defun org-ext/post-init-org ()
  (use-package org
    :defer t
    :config
    (setq org-hide-emphasis-markers t
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
        1 (compose-region (match-beginning 1) (match-end 1) ?∙))
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)))
    (dolist (i (number-sequence 1 org-n-level-faces))
      (set-face-attribute (intern (concat "org-level-" (number-to-string i))) nil
                          :weight 'bold))
    (add-hook 'org-todo-get-default-hook
              (lambda (mark _)
                (when (string-equal mark "NEXT")
                  (org-insert-schedule-&-deadline)
                  nil)))))

(defun org-ext/post-init-org-agenda ()
  (use-package org-agenda
    :defer t
    :config
    (setq org-agenda-window-setup 'current-window)
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      (kbd "C-j") #'org-agenda-next-item
      (kbd "C-k") #'org-agenda-previous-item)
    (advice-add #'org-agenda-list :after
                (lambda (&rest _)
                  (highlight-lines-matching-regexp "records:"
                                                   'org-agenda-calendar-record)))))

(defun org-ext/init-org-capture ()
  (use-package org-capture
    :config
    (setq org-directory (concat (getenv "HOME") "/Desktop/Org")
          org-default-notes-file (concat org-directory "/todos/" (format-time-string "%Y") ".org")
          org-capture-templates
          `(("t" "Todo" entry
             (file+headline ,org-default-notes-file ,(format-time-string "%b"))
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
             ,(concat "* %^{Note}"   "\n"
                      ":PROPERTIES:" "\n"
                      ":Created: %t" "\n"
                      ":END:"        "\n"
                      "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("p" "Protocol" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}"   "\n"
                      ":PROPERTIES:" "\n"
                      ":Created: %t" "\n"
                      ":Source: %c"  "\n"
                      ":END:"        "\n"
                      "\n"
                      "#+BEGIN_QUOTE" "\n"
                      "%i"            "\n"
                      "#+End_QUOTE"   "\n"
                      "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("L" "Protocol Link" entry
             (file+headline ,(concat org-directory "/notes/" (format-time-string "%Y") ".org")
                            ,(format-time-string "%b"))
             ,(concat "* %^{Note}"   "\n"
                      ":PROPERTIES:" "\n"
                      ":Created: %t" "\n"
                      ":Link: %c"    "\n"
                      ":END:"        "\n"
                      "\n"
                      "%?")
             :empty-lines 1
             :prepend t)
            ("r" "Recorder" entry
             (file+datetree ,(concat org-directory "/records.org"))
             "* %T %?")
            ("R" "Recorder with specified date" entry
             (file+datetree+prompt ,(concat org-directory "/records.org"))
             "* %T %?")))
    (spacemacs/set-leader-keys
      "aoc" nil
      "aoct" (defalias 'org-capture-todo (lambda () (interactive) (org-capture nil "t")))
      "aocn" (defalias 'org-capture-note (lambda () (interactive) (org-capture nil "n")))
      "aocr" (defalias 'org-capture-record (lambda () (interactive) (org-capture nil "r")))
      "aocR" (defalias 'org-capture-record-with-prompt (lambda () (interactive) (org-capture nil "R"))))
    (advice-add #'org-capture :before
                (lambda (&rest _)
                  "Remove a duplicates history."
                  (->> (all-completions "org-capture-template-prompt-history" obarray)
                       (--map (intern it))
                       (--filter (ignore-errors (symbol-value it)))
                       (--map (set it (-distinct (-remove #'s-blank?
                                                          (symbol-value it))))))))))

(defun org-ext/post-init-org-projectile ()
  (use-package org-projectile
    :defer t
    :config
    (advice-add #'org-projectile/capture :after
                (lambda (&rest _)
                  (let* ((target (org-capture-get :target))
                         (type (car target))
                         (target (cadr target)))
                    (cond
                     ((eq type 'function)
                      (-when-let (buf (save-window-excursion
                                        (funcall target)
                                        (current-buffer)))
                        (with-current-buffer buf
                          (add-hook 'after-save-hook
                                    (lambda ()
                                      (setq-default org-agenda-files (find-org-agenda-files)))
                                    nil 'local))))))))))

(defun org-ext/init-org-protocol ()
  (use-package org-protocol))

(defun org-ext/post-init-projectile ()
  (use-package projectile
    :after org-agenda
    :defer t
    :config
    (projectile-load-known-projects)
    (setq org-agenda-files (find-org-agenda-files))))

;;; packages.el ends here

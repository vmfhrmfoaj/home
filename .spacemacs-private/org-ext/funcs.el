(defun org-insert-schedule-&-deadline ()
  (org-schedule 'overwrite "+0d")
  (org-deadline 'overwrite))

(defun find-org-agenda-files ()
  (->> (shell-command-to-string (concat "find " org-directory "/"
                                        " -type f"
                                        " -name \"*.org\""))
       (s-split "\n")
       (--map (s-replace "//" "/" it))
       (--remove (s-blank? it))))

(defun remove-duplicated-org-tags-history (&rest _)
  (let ((tags (-flatten org-last-tags-completion-table)))
    (-update->> org-tags-history
                (-distinct)
                (-filter (lambda (tag)
                           (->> (split-string tag ":")
                                (-remove #'s-blank?)
                                (-every? (-partial #'-contains? tags))))))))

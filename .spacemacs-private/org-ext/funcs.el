(defun org-insert-schedule-&-deadline ()
  (org-schedule 'overwrite "+0d")
  (org-deadline 'overwrite))

(defun org-agenda-find-files ()
  (->> (shell-command-to-string (concat "find " org-directory "/"
                                        " -type f"
                                        " -name \"*.org\""))
       (s-split "\n")
       (--map (s-replace "//" "/" it))
       (--remove (s-blank? it))))

(defun org-protocol-cached-url (url)
  (concat org-cache-url-prefix (url-hexify-string url)))

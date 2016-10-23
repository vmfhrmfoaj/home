(defun org-insert-schedule-&-deadline ()
  (org-schedule 'overwrite "+0d")
  (org-deadline 'overwrite))

(defun find-org-agenda-files ()
  (let ((dirs (->> projectile-known-projects
                   (--remove (file-remote-p it))
                   (--filter (file-directory-p (concat it "/.git/")))
                   (--filter (string-match-p "/Desktop/" it))
                   (append (list org-directory))
                   (-map (-partial #'s-chop-suffix "/")))))
    (-mapcat (lambda (dir)
               (->> (shell-command-to-string (concat "find " dir "/"
                                                     " -maxdepth 3"
                                                     " -type f"
                                                     " -name \"*.org\""))
                    (s-split "\n")
                    (--map (s-replace "//" "/" it))
                    (--remove (s-blank? it))))
             dirs)))

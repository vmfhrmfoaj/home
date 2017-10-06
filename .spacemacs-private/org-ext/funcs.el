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

(defun org-protocol-cached-url? (url)
  (string-match-p (concat "^" (regexp-quote org-cache-url-prefix)) url))

(defun org-url=>cached-url (&optional elem)
  (interactive)
  (let ((elem (or elem (org-element-context))))
    (when (eq 'link (car elem))
      (let* ((elem (cadr elem))
             (type (plist-get elem :format))
             (link (plist-get elem :raw-link))
             (s (+ (plist-get elem :begin) 2))
             (e (- (plist-get elem :contents-begin) 2)))
        (when (eq 'bracket type)
          (let ((url (buffer-substring-no-properties s e)))
            (unless (org-protocol-cached-url? url)
              (kill-region s e)
              (goto-char s)
              (insert (org-protocol-cached-url url)))))))))

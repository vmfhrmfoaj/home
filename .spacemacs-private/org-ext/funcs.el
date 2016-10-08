(defun org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

(defun org-capture-note ()
  (interactive)
  (org-capture nil "n"))

(defun org-capture-journal ()
  (interactive)
  (org-capture nil "j"))

(defun org-capture-journal-with-prompt ()
  (interactive)
  (org-capture nil "J"))

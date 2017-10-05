(defface org-cancelled
  '((t (:foreground "red" :weight bold)))
  "Face used for todo keywords that indicate CANCELLED items."
  :group 'org-faces)

(defface org-next
  '((t (:foreground "dark green" :weight bold)))
  "Face used for todo keywords that indicate CANCELLED items."
  :group 'org-faces)

(defface org-agenda-calendar-record
  '((t (:height 0.9 :inherit (shadow default))))
  "Face used to show events and appointments in the agenda."
  :group 'org-faces)

(defcustom org-capture-use-cached-url t
  "Non-nil means using Google Web Cache when captured by org-protocol."
  :group 'org-protocol
  :type 'boolean)

(defcustom org-cache-url-prefix "http://webcache.googleusercontent.com/search?q=cache:"
  "TODO"
  :group 'org
  :type 'string)

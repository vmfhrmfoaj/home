(defmacro -update-> (&rest thread)
  "TODO"
  `(setq ,(-first-item thread) (->  ,@thread)))

(defmacro -update->> (&rest thread)
  "TODO"
  `(setq ,(-first-item thread) (->> ,@thread)))


(require 'color)

(defun dim-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-darken-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(defun light-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-lighten-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(defun saturate-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-saturate-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(setq-default font-lock--skip nil)
(make-local-variable 'font-lock--skip)

(defun in-comment? ()
  "TODO"
  (comment-only-p (save-excursion
                    (goto-char (match-beginning 0))
                    (point-at-bol))
                  (point)))

(defun safe-up-list-1 ()
  "TODO"
  (condition-case nil
      (up-list)
    (error (setq-local font-lock--skip t))))

(defun safe-down-list-1 ()
  "TODO"
  (condition-case nil
      (down-list)
    (error (setq-local font-lock--skip t))))

(defun safe-regexp? (regex)
  "TODO"
  (condition-case nil
      (progn (string-match-p regex "") t)
    (error nil)))


(defun pixel->frame-unit (pixel)
  "TODO"
  (round (/ pixel (/ (float (frame-pixel-width)) (frame-width)))))

(defun frame-unit->pixel (frame-unit)
  "TODO"
  (round (* frame-unit (/ (float (frame-pixel-width)) (frame-width)))))

(defun custom-display-pixel-width ()
  "TODO"
  (->> (--filter (-when-let (frames (-> (assoc 'frames it) cdr))
                   (--some? (eq (selected-frame) it) frames))
                 (display-monitor-attributes-list))
       (-first-item)
       (assoc 'geometry)
       (cdr)
       (nth 2)))


(defun include-shell-var-in (file)
  "TODO"
  (dolist (it (->> (concat "EMACS_INIT=1 source " file "; env")
                   (shell-command-to-string)
                   (s-lines)
                   (--map (s-split "=" it))))
    (let ((key (car it))
          (val (cadr it)))
      (setenv key val))))


(defvar rsync-retry-coutner 3
  "TODO")

(defvar rsync-remote-dir nil
  "TODO")

(defvar rsync-remote-opts "-z"
  "TODO")

(defvar rsync-remote-ssh-opts "-T -o Compression=no -x"
  "TODO")

(defvar rsync-remote-notify-cmd
  (cond ((eq system-type 'gnu/linux)
         "notify-send Emacs ")
        ((eq system-type 'darwin)
         "terminal-notifier -title Emacs -message "))
  "TODO")

(defun rsync-remote-dir (&optional buf)
  "TODO"
  (let* ((buf (or (and buf (get-buffer buf))
                  (current-buffer)))
         (buf-name (buffer-name buf))
         (path (buffer-file-name buf)))
    (-when-let (root (and rsync-remote-dir path
                          (car (dir-locals-find-file path))))
      (let* ((remote-root (s-chop-suffix "/" rsync-remote-dir))
             (remote-path (->> root
                               (file-relative-name path)
                               (concat remote-root "/"))))
        (if (not (fboundp 'async-start))
            (copy-file path remote-path t)
          (setq-local rsync-retry-coutner (1- rsync-retry-coutner))
          (async-start
           `(lambda ()
              (let ((err-buf-name "*err*")
                    (cmd (concat "rsync "
                                 ,rsync-remote-opts
                                 " -e \"ssh " ,rsync-remote-ssh-opts "\" "
                                 ,path " "
                                 ,remote-path)))
                (if (= 0 (shell-command cmd nil err-buf-name))
                    t
                  (concat "Error ouccurred while syncing '"
                          ,buf-name "': " cmd "; "
                          (when (get-buffer err-buf-name)
                            (with-current-buffer err-buf-name
                              (goto-char (point-min))
                              (buffer-substring (point-min) (line-end-position))))))))
           `(lambda (res)
              (if (not (stringp res))
                  (message (concat "syncing '" ,buf-name "' is done!"))
                (message res)
                (if (<= 0 ,rsync-retry-coutner)
                    (rsync-remote-dir ,buf-name)
                  (shell-command-to-string
                   (concat rsync-remote-notify-cmd "'" res "'")))))))))))

(defvar buf-visit-time nil
  "TODO")
(make-local-variable 'buf-visit-time)

(defun update-buf-visit-time (&rest _)
  "TODO"
  (ignore-errors
    (let ((cur-win (selected-window))
          (cur-time (current-time)))
      (if (window-dedicated-p cur-win)
          (setq buf-visit-time nil)
        (-update->> buf-visit-time
                    (-partition 2)
                    (--filter (window-live-p (-first-item it)))
                    (-mapcat #'identity))
        (-update-> buf-visit-time
                   (plist-put cur-win cur-time))))))

(defun buf-visit-time (&optional buf win)
  "TODO"
  (ignore-errors
    (with-current-buffer (or buf (current-buffer))
      (if (and (< 1 (length (window-list)))
               (not (eq 'all win)))
          (plist-get buf-visit-time (or win (selected-window)))
        (-some->> buf-visit-time
                  (-partition 2)
                  (-map #'-second-item)
                  (--sort (time-less-p other it))
                  (-first-item))))))


(defvar exclude-alt-buf-regex ""
  "TODO")

(defun switch-to-previous-buffer (&optional win)
  "TODO"
  (interactive)
  (unless (window-dedicated-p)
    (let ((cur-buf (current-buffer)))
      (-when-let (prev-buf (or (->> (helm-buffer-list)
                                    (--remove (string-match-p exclude-alt-buf-regex it))
                                    (-map #'get-buffer)
                                    (-remove #'minibufferp)
                                    (--remove-first (eq cur-buf it))
                                    (--sort (let ((it    (or (buf-visit-time it)    0))
                                                  (other (or (buf-visit-time other) 0)))
                                              (time-less-p other it)))
                                    (-first-item))))
        (set-window-buffer (selected-window) prev-buf)))))


(defun get-scratch-buffer-create ()
  "TODO"
  (interactive)
  (pop-to-buffer (get-buffer-create "*scratch*"))
  (unless (eq 'org-mode major-mode)
    (org-mode)))

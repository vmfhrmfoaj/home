(when window-system
  (defvar main-monitor
    (let ((get-resolution (lambda (it) (->> it (nth 1) (-take-last 2) (apply #'*)))))
      (-some->> (x-display-monitor-attributes-list)
                (--max-by (> (funcall get-resolution it)
                             (funcall get-resolution other)))))
    "See `display-monitor-attributes-list'")

  (defvar main-monitor-resolution
    (-some->> main-monitor
              (assoc 'geometry)
              (-drop 1)
              (-take-last 2))
    "See `display-monitor-attributes-list'")

  (defvar main-monitor-mm-size
    (-some->> main-monitor
              (assoc 'mm-size)
              (-drop 1))
    "See `display-monitor-attributes-list'"))

(defvar null-fn (-const nil)
  "TODO")

(defmacro -update-> (&rest thread)
  "TODO"
  `(setq ,(-first-item thread) (->  ,@thread)))

(defmacro -update->> (&rest thread)
  "TODO"
  `(setq ,(-first-item thread) (->> ,@thread)))

(setq byte-compile-warnings nil)

(defun byte-compile-with-thread (symbol)
  "TODO"
  (make-thread
   `(lambda ()
      (byte-compile #',symbol))))
(byte-compile #'byte-compile-with-thread)

(defmacro defn (name args &optional docstr &rest body)
  "TODO"
  (declare (doc-string 3) (indent 2))
  `(prog1
       ,(if docstr
            `(defun ,name ,args ,docstr ,@body)
          `(defun ,name ,args ,@body))
    (byte-compile-with-thread #',name)))


(require 'color)

(defn color-rgb-to-hex-2-dig (R G B)
  (color-rgb-to-hex R G B 2))

(defn dim-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-darken-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defn light-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-lighten-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defn saturate-color (color p)
  "TODO"
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-saturate-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defn mix-color (color-a color-b)
  "TODO"
  (let ((rgb-a (color-name-to-rgb color-a))
        (rgb-b (color-name-to-rgb color-b)))
    (->> (-zip-with #'+ rgb-a rgb-b)
         (--map (/ it 2))
         (apply #'color-rgb-to-hex-2-dig))))

(defn mix-colors (color &rest colors)
  "TODO"
  (if (not colors)
      color
    (-reduce-from #'mix-color color colors)))

(defn custom-face-attribute (face attr)
  "TODO"
  (let ((val (face-attribute face attr)))
    (if (not (eq val 'unspecified))
        val
      (let ((parents (face-attribute face :inherit)))
        (unless (eq parents 'unspecified)
          (catch 'break
            (dolist (p (-list parents))
              (-when-let (val (custom-face-attribute p attr))
                (throw 'break val)))
            (face-attribute 'default attr)))))))

(defn color-from (face attr &optional p)
  "TODO"
  (let ((color (custom-face-attribute face attr)))
    (if (not p)
        color
      (let ((fn (cond
                 ((< 0 p) #'light-color)
                 ((< p 0) #'dim-color)
                 ((= p 0) (lambda (color _) color))))
            (p (abs p)))
        (funcall fn color p)))))


(defn in-comment? ()
  "TODO"
  (comment-only-p (save-excursion
                    (goto-char (match-beginning 0))
                    (point-at-bol))
                  (point)))

(setq-default font-lock--skip nil)
(make-local-variable 'font-lock--skip)

(defn safe-up-list-1 ()
  "TODO"
  (condition-case nil
      (up-list)
    (error (setq font-lock--skip t))))

(defn safe-down-list-1 ()
  "TODO"
  (condition-case nil
      (down-list)
    (error (setq font-lock--skip t))))

(defn safe-regexp? (regex)
  "TODO"
  (condition-case nil
      (progn (string-match-p regex "") t)
    (error nil)))


(defn pixel->frame-unit (pixel)
  "TODO"
  (round (/ pixel (/ (float (frame-pixel-width)) (frame-width)))))

(defn frame-unit->pixel (frame-unit)
  "TODO"
  (round (* frame-unit (/ (float (frame-pixel-width)) (frame-width)))))

(defn custom-display-pixel-width ()
  "TODO"
  (->> (--filter (-when-let (frames (-> (assoc 'frames it) cdr))
                   (--some? (eq (selected-frame) it) frames))
                 (display-monitor-attributes-list))
       (-first-item)
       (assoc 'geometry)
       (cdr)
       (nth 2)))

(defvar rsync-retry-coutner 3
  "TODO")

(defvar rsync-remote-dir nil
  "e.g, user@192.168.0.1:~/")

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

(defvar rsync-ignore-patterns '(".git" ".svn")
  "TODO")

(defn rsync-remote-dir (&optional buf)
  "TODO"
  (let* ((buf (or (and buf (get-buffer buf))
                  (current-buffer)))
         (buf-name (buffer-name buf))
         (path (buffer-file-name buf))
         (ignore-regex (concat "/" (regexp-opt rsync-ignore-patterns) "\\(/\\|$\\)")))
    (-when-let (root (and rsync-remote-dir path
                          (not (string-match-p ignore-regex path))
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
                    (cmd (concat "SSH_AUTH_SOCK=" ,(getenv "SSH_AUTH_SOCK") " "
                                 "rsync "
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

(defn update-buf-visit-time (&rest _)
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

(defn buf-visit-time (buf &optional win)
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

(defn switch-to-previous-buffer (&optional win)
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
        (switch-to-buffer prev-buf nil t)))))


(defn get-scratch-buffer-create ()
  "TODO"
  (interactive)
  (pop-to-buffer (get-buffer-create "*scratch*"))
  (unless (eq 'org-mode major-mode)
    (org-mode))
  (when (fboundp #'persp-add-buffer-without-switch)
    (persp-add-buffer-without-switch)))

(defn kill-new-buffer-file-name ()
  "TODO"
  (interactive)
  (-when-let (file-name (buffer-file-name))
    (message (kill-new file-name))))


(defn enabled? (mode-status)
  "TODO"
  (cond ((symbolp mode-status) mode-status)
        ((numberp mode-status) (not (zerop mode-status)))
        (t nil)))

(defn disable-modes (modes)
  "TODO"
  (--map (and (symbol-value it)
              (funcall it 0))
         modes))

(defn restore-modes (modes status)
  "TODO"
  (--map (and (cdr it)
              (funcall (car it) (cdr it)))
         (-zip modes status)))

(defmacro with-disable-modes (modes &rest body)
  "TODO"
  `(let ((mode-status (-map #'symbol-value ,modes)))
     (disable-modes ,modes)
     (unwind-protect
         (prog1 (progn ,@body)
           (restore-modes ,modes mode-status))
       (restore-modes ,modes mode-status))))

(put 'with-disable-modes 'lisp-indent-function 'defun)

(defn comment-it ()
  "TODO"
  (interactive)
  (if (region-active-p)
      (call-interactively #'comment-dwim)
    (save-excursion (comment-line 1))
    (beginning-of-line-text)))

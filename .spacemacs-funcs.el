;;-*- byte-compile-dynamic: t; -*-

(eval-when-compile
  (dolist (dir '("~/.emacs.d/elpa/"
                 "~/.emacs.d/elpa/develop"))
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path)))
  (require 'dash))

(defun pixel->frame-unit (pixel)
  (round (/ pixel (/ (float (frame-pixel-width)) (frame-width)))))

(defun frame-unit->pixel (frame-unit)
  (round (* frame-unit (/ (float (frame-pixel-width)) (frame-width)))))

(defun custom-display-pixel-width ()
  (->> (--filter (-when-let (frames (-> (assoc 'frames it) cdr))
                   (--some? (eq (selected-frame) it) frames))
                 (display-monitor-attributes-list))
       (-first-item)
       (assoc 'geometry)
       (cdr)
       (nth 2)))

(defun include-shell-var-in (file)
  (dolist (it (->> (concat "source " file "; env")
                   (shell-command-to-string)
                   (s-lines)
                   (--map (s-split "=" it))))
    (let ((key (car it))
          (val (cadr it)))
      (setenv key val))))

(defmacro -update-> (&rest thread)
  `(setq ,(-first-item thread) (->  ,@thread)))

(defmacro -update->> (&rest thread)
  `(setq ,(-first-item thread) (->> ,@thread)))

(defun enabled? (mode-status)
  (cond ((symbolp mode-status) mode-status)
        ((numberp mode-status) (not (zerop mode-status)))
        (t nil)))

(defun disable-modes (modes)
  (--map (and (symbol-value it)
              (funcall it 0))
         modes))

(defun restore-modes (modes status)
  (--map (and (cdr it)
              (funcall (car it) (cdr it)))
         (-zip modes status)))

(defmacro with-disable-modes (modes &rest body)
  `(let ((mode-status (-map #'symbol-value ,modes)))
     (disable-modes ,modes)
     (unwind-protect
         (prog1 (progn ,@body)
           (restore-modes ,modes mode-status))
       (restore-modes ,modes mode-status))))

(put 'with-disable-modes 'lisp-indent-function 'defun)

(defun advice-disable-modes (modes f)
  (advice-add f :around
              (lexical-let ((modes modes))
                (lambda (f &rest args)
                  "Added by `advice-disable-modes'."
                  (with-disable-modes modes
                    (apply f args))))))

(setq auto-indent-skip-when-open-file t)

(defun auto-indent (&rest _)
  "Auto indent for `evil-mode'"
  (unless auto-indent-skip-when-open-file
    (save-match-data
      (save-mark-and-excursion
       (when (and (derived-mode-p 'prog-mode)
                  (not buffer-read-only)
                  (>= auto-indent-block-level 1))
         (let ((start
                (progn
                  (condition-case nil
                      (progn
                        (backward-up-list 1 t)
                        (ignore-errors
                          (dotimes (_ (1- auto-indent-block-level))
                            (backward-up-list 1 t)))
                        (point))
                    (error nil))))
               (end
                (progn
                  (condition-case nil
                      (progn
                        (forward-list)
                        (point))
                    (error nil)))))
           (when (and start end)
             (let ((fancy-narrow--beginning nil)
                   (fancy-narrow--end nil))
               (indent-region start end))))))))
  (setq-local auto-indent-skip-when-open-file nil))

(defun set-window-buffer+ (set-win-buf wind buf &optional opt)
  (when (and (->> (window-list)
                  (-remove (-partial #'eq (selected-window)))
                  (-map #'window-buffer)
                  (-some? (-partial #'eq buf)))
             (->> this-command
                  (format "%s")
                  (string-match-p "quit\\|bury")
                  (not)))
    (funcall set-win-buf
             (->> (window-list)
                  (--remove (eq (selected-window) it))
                  (--filter (eq buf (window-buffer it)))
                  (-first-item))
             (window-buffer wind) opt))
  (funcall set-win-buf wind buf opt))

(defun dim-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-darken-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(defun light-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-lighten-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(defun saturate-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-saturate-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex)))

(defun in-comment? ()
  (comment-only-p (save-excursion
                    (goto-char (match-beginning 0))
                    (point-at-bol))
                  (point)))

(defun safe-up-list-1 ()
  (condition-case nil
      (up-list)
    (error (setq-local font-lock--skip t))))

(defun safe-down-list-1 ()
  (condition-case nil
      (down-list)
    (error (setq-local font-lock--skip t))))

(defun safe-regexp? (regex)
  (condition-case nil
      (progn (string-match-p regex "") t)
    (error nil)))

(setq rsync-retry-coutner 3
      rsync-remote-dir nil
      rsync-remote-opts "-z"
      rsync-remote-ssh-opts "-T -o Compression=no -x"
      rsync-remote-notify-cmd
      (cond ((eq system-type 'gnu/linux)
             "notify-send Emacs ")
            ((eq system-type 'darwin)
             "terminal-notifier -title Emacs -message ")))

(defun rsync-remote-dir (&optional buf)
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

(setq buf-visit-time nil)

(defun update-buf-visit-time (&rest _)
  (ignore-errors
    (let ((cur-win (selected-window))
          (cur-time (current-time)))
      (if (window-dedicated-p cur-win)
          (setq buf-visit-time nil)
        (make-local-variable 'buf-visit-time)
        (-update->> buf-visit-time
                    (-partition 2)
                    (--filter (window-live-p (-first-item it)))
                    (-mapcat #'identity))
        (-update-> buf-visit-time
                   (plist-put cur-win cur-time))))))

(defun buf-visit-time (&optional buf win)
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

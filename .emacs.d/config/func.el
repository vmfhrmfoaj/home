;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package dash
  :ensure t
  :config
  (put '->  'lisp-indent-function 0)
  (put '--> 'lisp-indent-function 0)
  (put '->> 'lisp-indent-function 0)
  (put '-some->  'lisp-indent-function 0)
  (put '-some--> 'lisp-indent-function 0)
  (put '-some->> 'lisp-indent-function 0))

(use-package s
  :ensure t)

(defvar null-fn (-const nil))

(defmacro -update-> (&rest thread)
  (declare (indent 0))
  `(setq ,(-first-item thread) (->  ,@thread)))

(defmacro -update->> (&rest thread)
  (declare (indent 0))
  `(setq ,(-first-item thread) (->> ,@thread)))


(require 'color)

(defun color-rgb-to-hex-2-dig (R G B)
  (color-rgb-to-hex R G B 2))

(defun dim-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-darken-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defun light-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-lighten-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defun saturate-color (color p)
  (->> color
       (color-name-to-rgb)
       (apply #'color-rgb-to-hsl)
       (apply (-rpartial #'color-saturate-hsl p))
       (apply #'color-hsl-to-rgb)
       (apply #'color-rgb-to-hex-2-dig)))

(defun mix-color (color-a color-b)
  (let ((rgb-a (color-name-to-rgb color-a))
        (rgb-b (color-name-to-rgb color-b)))
    (->> (-zip-with #'+ rgb-a rgb-b)
         (--map (/ it 2))
         (apply #'color-rgb-to-hex-2-dig))))

(defun mix-colors (color &rest colors)
  (if (not colors)
      color
    (-reduce-from #'mix-color color colors)))

(defun custom-face-attribute (face attr)
  (if (listp face)
      (let ((val (plist-get face attr)))
        (if (or (null val) (eq 'unspecified val))
            (catch 'stop
              (dolist (face (-list (plist-get face :inherit)))
                (let ((val (custom-face-attribute face attr)))
                  (when (and val (not (eq 'unspecified val)))
                    (throw 'stop val)))))
          val))
    (face-attribute face attr nil t)))

(defun color-from (face attr &optional p s)
  (let ((color (custom-face-attribute face attr)))
    (if (or (not (memq attr '(:foreground :background)))
            (eq 'unspecified color))
        'unspecified
      (when (listp color)
        (setq color (plist-get color :color)))
      (when p
        (let ((fn (cond
                   ((< 0 p) #'light-color)
                   ((< p 0) #'dim-color)
                   ((= p 0) (lambda (color _) color))))
              (p (abs p)))
          (setq color (funcall fn color p))))
      (when s
        (setq color (saturate-color color s)))
      color)))

(defun fg-color-from (face &optional p s)
  (color-from face :foreground p s))

(defun bg-color-from (face &optional p s)
  (color-from face :background p s))


(defun in-string? (&optional pos)
  (let ((pos (or pos (point)))
        (ppss (save-excursion (syntax-ppss pos))))
    (not (not (nth 3 ppss)))))

(defun in-comment? (&optional pos)
  (let ((pos (or pos (point)))
        (ppss (save-excursion (syntax-ppss pos))))
    (not (not (nth 4 ppss)))))

(defun in-comment-or-string? (&optional pos)
  (let ((pos (or pos (point)))
        (ppss (save-excursion (syntax-ppss pos))))
    (or (not (not (nth 3 ppss)))
        (not (not (nth 4 ppss))))))

(setq-default font-lock--skip nil)
(make-local-variable 'font-lock--skip)

(defun safe-up-list-1 ()
  (condition-case nil
      (up-list)
    (error (setq font-lock--skip t))))

(defun safe-down-list-1 ()
  (condition-case nil
      (down-list)
    (error (setq font-lock--skip t))))

(defun safe-regexp? (regex)
  (condition-case nil
      (progn (string-match-p regex "") t)
    (error nil)))


(defvar rsync-retry-coutner 3)

(defvar rsync-remote-dir nil
  "e.g, user@192.168.0.1:~/")

(defvar rsync-remote-opts "-z")

(defvar rsync-remote-ssh-opts "-T -o Compression=no -x")

(defvar rsync-remote-notify-cmd
  (cond ((eq system-type 'gnu/linux)
         "notify-send Emacs ")
        ((eq system-type 'darwin)
         "terminal-notifier -title Emacs -message ")))

(defvar rsync-ignore-patterns '(".git" ".svn"))

(defun rsync-remote-dir (&optional buf)
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

(defvar-local buf-visit-time nil)

(defun update-buf-visit-time (&rest _)
  (setq buf-visit-time (current-time)))

(defun buf-visit-time (buf)
  (condition-case err
      (let ((buf (if (stringp buf)
                     (get-buffer buf)
                   buf)))
        (with-current-buffer (or buf (current-buffer))
          buf-visit-time))
    (error (error err))))


(defvar include-prev-buf-regex "^$")
(defvar exclude-prev-buf-regex "^$")

(defun sort-buffer-by-visit-time (bufs)
  (-some->> bufs
    (--sort (let ((it    (or (buf-visit-time it)    0))
                  (other (or (buf-visit-time other) 0)))
              (time-less-p other it)))))

(defun switch-to-previous-buffer-in (bufs)
  (let ((visible-bufs (->> (frame-list)
                        (--mapcat (with-selected-frame it (window-list)))
                        (-map #'window-buffer))))
    (when-let ((bufs (->> bufs
                       (--filter (buffer-live-p it))
                       (--remove (or (minibufferp it)
                                     (let ((buf-name (buffer-name it)))
                                       (and (string-match-p exclude-prev-buf-regex buf-name)
                                            (not (string-match-p include-prev-buf-regex buf-name))))))
                       (sort-buffer-by-visit-time))))
      (if-let ((prev-buf (->> bufs
                           (--remove (-contains? visible-bufs it))
                           (-first-item))))
          (progn
            (switch-to-buffer prev-buf nil t)
            prev-buf)
        (let ((visible-prev-buf (-first-item bufs)))
          (dolist (frame (frame-list))
            (with-selected-frame frame
              (when-let ((win (get-buffer-window visible-prev-buf)))
                (select-frame-set-input-focus frame)
                (select-window win))))
          visible-prev-buf)))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-previous-buffer-in (buffer-list)))


(defvar scratch-buffer-name "*scratch*")
(defvar scratch-buffer-temp-file "~/.emacs.d/scratch")

(defvar scratch-major-mode 'text-mode)

(defun pop-to-scratch-buffer ()
  (interactive)
  (unless (or (window-in-direction 'up)
              (window-in-direction 'down))
    (split-window-vertically)
    (other-window 1))
  (let ((buf (get-buffer-create scratch-buffer-name)))
    (with-current-buffer buf
      (unless (eq scratch-major-mode major-mode)
        (funcall scratch-major-mode)
        (when (file-exists-p scratch-buffer-temp-file)
          (insert-file-contents scratch-buffer-temp-file))
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when-let ((buf (stringp scratch-buffer-temp-file)))
                      (write-region (point-min) (point-max) scratch-buffer-temp-file)))
                  nil 'local)
        (setq-local default-directory (concat home-dir "/Desktop/Org/")
                    projectile-project-name "Org"))
      (when (= (point-min) (point-max))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun kill-new-buffer-file-name ()
  (interactive)
  (-when-let (file-name (buffer-file-name))
    (message (kill-new file-name))))

;; from Magnars
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

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
         (-zip-pair modes status)))

(defmacro with-disable-modes (modes &rest body)
  `(let ((mode-status (-map #'symbol-value ,modes)))
     (disable-modes ,modes)
     (unwind-protect
         (prog1 (progn ,@body)
           (restore-modes ,modes mode-status))
       (restore-modes ,modes mode-status))))

(put 'with-disable-modes 'lisp-indent-function 'defun)

(defun comment-it ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'comment-dwim)
    (save-excursion (comment-line 1))
    (beginning-of-line-text)))


(defvar-local show-error-list-fn (lambda () (message "Not configured")))

(defun show-error-list ()
  (interactive)
  (funcall-interactively show-error-list-fn))


(defun open-link-at-point ()
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (org-open-at-point))
   ((string= "*lsp-help*" (buffer-name))
    (-some--> (get-text-property (point) 'help-echo)
              (and (string-match-p goto-address-url-regexp it)
                   (browse-url it))))
   (t (message (concat "Don't know how to open a link on '" (symbol-name major-mode) "'")))))

(defun format-buffer-or-region ()
  (interactive)
  (cond
   (lsp-mode
    (if (use-region-p)
        (call-interactively #'lsp-format-region)
      (call-interactively #'lsp-format-buffer)))
   (t (message (concat "Don't know how to format a file on '" (symbol-name major-mode) "'")))))


(defun kill-buffer-and-delete-window ()
  (interactive)
  (if (fboundp 'projectile-kill-buffer)
      (projectile-kill-buffer)
    (kill-buffer))
  (when (or (window-in-direction 'up)
            (window-in-direction 'down))
    (delete-window)))

(provide 'func)

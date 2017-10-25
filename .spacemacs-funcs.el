(defun pixel->frame-unit (pixel)
  (round (/ pixel (/ (float (frame-pixel-width)) (frame-width)))))

(defun frame-unit->pixel (frame-unit)
  (round (* frame-unit (/ (float (frame-pixel-width)) (frame-width)))))

(defun custom-display-pixel-width ()
  (->> (--filter (-when-let (frames (-> (assoc 'frames it) cdr))
                   (--some? (eq (selected-frame) it) frames))
                 (display-monitor-attributes-list))
       (first)
       (assoc 'geometry)
       (cdr)
       (nth 2)))


(defun resolve-sh-var (str)
  (while (string-match (concat "\\$\\([_a-zA-Z0-9]+\\|[({].+[})]\\)") str)
    (let* ((var (match-string 1 str))
           (res (save-match-data
                  (->> var
                       (concat "echo $")
                       (shell-command-to-string)
                       (s-trim)))))
      (setq str (replace-match res t nil str))))
  str)

(defun include-shell-var-in (file)
  (when (file-exists-p file)
    (let* ((regx "export\\s-+\\([^=]+\\)=\"?\\(.+?\\)\"?$")
           (exports (->> (with-temp-buffer
                           (insert-file-contents file)
                           (split-string (buffer-string) "\n" t))
                         (--filter (not (string-match-p "^#" it)))
                         (--filter (string-match-p regx it))
                         (--map (replace-regexp-in-string "\\\\" "" it)))))
      (dolist (it exports)
        (string-match regx it)
        (let ((key   (match-string-no-properties 1 it))
              (value (match-string-no-properties 2 it)))
          (setenv key (resolve-sh-var value)))))))


(defmacro -update->> (&rest thread)
  `(setq ,(first thread) (-some->> ,@thread)))


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


(setq-default auto-indent-skip-when-open-file t)

(setq-default auto-indent-block-level 1)

(defun auto-indent (&rest _)
  "auto-indent-for-evil-mode"
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
                        (backward-up-list)
                        (ignore-errors
                          (dotimes (_ (1- auto-indent-block-level))
                            (backward-up-list)))
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
             (indent-region start end)))))))
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
    (setq-local font-lock--skip t)))

(defun safe-down-list-1 ()
  (condition-case nil
      (down-list)
    (setq-local font-lock--skip t)))


(setq tramp-sync-dir nil)

(defun tramp-sync ()
  (when (and tramp-sync-dir buffer-file-name)
    (-when-let (root (car (dir-locals-find-file buffer-file-name)))
      (let ((path (->> root
                       (file-relative-name buffer-file-name)
                       (concat tramp-sync-dir "/"))))
        (if (fboundp 'async-start)
            (async-start
             `(lambda ()
                (copy-file ,buffer-file-name ,path)))
          (copy-file buffer-file-name path))))))

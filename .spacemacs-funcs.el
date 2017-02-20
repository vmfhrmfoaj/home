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
(defun auto-indent (&rest _)
  "auto-indent-for-evil-mode"
  (unless auto-indent-skip-when-open-file
    (save-match-data
      (save-mark-and-excursion
       (when (and (derived-mode-p 'prog-mode)
                  (not buffer-read-only))
         (let ((start (progn
                        (beginning-of-defun)
                        (point)))
               (end   (progn
                        (end-of-defun)
                        (point))))
           (indent-region start end))))))
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

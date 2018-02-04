(setq helm-dumb-jump--keyword nil
      helm-dumb-jump--proj nil)

(defun helm-dump-jump--action (find-file-fn candidate)
  (let* ((candidate (helm-grep-split-line candidate))
         (file (nth 0 candidate))
         (line (nth 1 candidate)))
    (if (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack)
      (ring-insert find-tag-marker-ring (point-marker)))
    (funcall find-file-fn (concat helm-dumb-jump--proj "/" file))
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line)))
      (beginning-of-line)
      (when (re-search-forward (regexp-quote helm-dumb-jump--keyword)
                               (line-end-position) 'noerr)
        (goto-char (match-beginning 0))))
    (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
      (run-hooks 'dumb-jump-after-jump-hook))))

(defun helm-dump-jump--insert-file (file)
  (let ((def-dir default-directory))
    (switch-to-buffer (get-buffer-create "*helm-dumb-jump: persistent*"))
    (erase-buffer)
    (setq buffer-file-name file
          default-directory def-dir)
    (insert-file-contents file)
    (set-auto-mode)
    (font-lock-fontify-buffer)))

(defun helm-dump-jump--persistent-action (candidate)
  (helm-dump-jump--action #'helm-dump-jump--insert-file candidate))

(defun custom-helm-swoop--get-content (buf &optional linum)
  (let* (ret
         (buf (or (get-buffer buf) (current-buffer)))
         ;; NOTE:
         ;;  a advice of the compiled fundamental functions is not working.
         ;;  see http://nullprogram.com/blog/2013/01/22/
         (pos-min (or fancy-narrow--beginning (point-min)))
         (pos-max (or fancy-narrow--end (point-max)))
         (str (with-current-buffer buf
                (buffer-substring-no-properties pos-min pos-max)))
         (num (line-number-at-pos pos-min))
         (fmt (concat "%-"
                      (-> pos-max
                          (line-number-at-pos)
                          (number-to-string)
                          (length)
                          (number-to-string))
                      "s "))
         (colorize (byte-compile (lambda (it) (propertize it 'font-lock-face 'helm-swoop-line-number-face))))
         (insert-linum (-compose #'insert
                                 (if helm-swoop-use-line-number-face
                                     colorize
                                   #'identity)
                                 (-partial #'format fmt))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (funcall insert-linum num)
      (while (progn (forward-line) (not (eobp)))
        (-update-> num (1+))
        (if (and (not linum)
                 (looking-at-p "^[0-9]+\\s-*$"))
            (kill-line)
          (funcall insert-linum num)))
      (setq ret (buffer-substring (point-min) (point-max))))
    ret))

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

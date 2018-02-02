(require 'dash-functional nil 'noerr)

(unless (fboundp '-rpartial)
  (defun -rpartial (fn &rest args)
    (lexical-let ((fn fn) (args args))
      (lambda (&rest args-before) (apply fn (append args-before args))))))

(defun get-text-properties (start end property)
  (let (props)
    (while (-when-let (pos (text-property-not-all start end property nil))
             (add-to-list 'props (list pos property (get-text-property pos property)))
             (setq start (1+ pos))
             pos))
    props))

(defun put-text-properties (props)
  (dolist (prop props)
    (let ((pos  (car   prop))
          (prop (cadr  prop))
          (val  (caddr prop)))
      (put-text-property pos (1+ pos) prop val))))

(defmacro without-text-property (start end property &rest body)
  `(let ((props (get-text-properties ,start ,end ,property)))
     (with-silent-modifications
       (remove-text-properties ,start ,end (list ,property nil))
       (unwind-protect
           (prog1 (progn ,@body)
             (put-text-properties props))
         (put-text-properties props)))))

(defmacro without-text-property-hard (start end property &rest body)
  `(progn
     (with-silent-modifications
       (remove-text-properties ,start ,end (list ,property nil)))
     (unwind-protect
         (prog1 (progn ,@body)
           (font-lock-flush ,start ,end))
       (font-lock-flush))))

(defmacro without-text-properties-hard (start end properties &rest body)
  `(progn
     (with-silent-modifications
       (dolist (property ,properties)
         (remove-text-properties ,start ,end (list property nil))))
     (unwind-protect
         (prog1 (progn ,@body)
           (font-lock-flush ,start ,end))
       (font-lock-flush))))

(put 'without-text-property        'lisp-indent-function 'defun)
(put 'without-text-property-hard   'lisp-indent-function 'defun)
(put 'without-text-properties-hard 'lisp-indent-function 'defun)

(defun custom-helm-swoop--get-content (buf &optional linum)
  (let* (ret
         (buf (or (get-buffer buf) (current-buffer)))
         ;; NOTE:
         ;;  a advice of the compiled fundamental functions is not working.
         ;;  see http://nullprogram.com/blog/2013/01/22/
         (pos-min (or fancy-narrow--beginning (point-min)))
         (pos-max (or fancy-narrow--end (point-max)))
         (str (helm-swoop--buffer-substring pos-min pos-max))
         (num (line-number-at-pos pos-min))
         (fmt (or (and (boundp 'linum-relative-format) (concat linum-relative-format " "))
                  (and (boundp 'linum-format)          (concat linum-format " "))
                  "%s "))
         (colorize (lambda (it) (propertize it 'font-lock-face 'helm-swoop-line-number-face)))
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
      (setq ret (helm-swoop--buffer-substring (point-min) (point-max))))
    ret))

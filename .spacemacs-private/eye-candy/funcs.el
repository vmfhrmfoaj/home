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

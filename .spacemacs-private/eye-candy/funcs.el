(require 'dash-functional nil 'noerr)

(unless (fboundp '-rpartial)
  (defun -rpartial (fn &rest args)
    (lexical-let ((fn fn) (args args))
      (lambda (&rest args-before) (apply fn (append args-before args))))))

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

(put 'without-text-property 'lisp-indent-function 'defun)

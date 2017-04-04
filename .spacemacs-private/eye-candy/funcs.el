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

(defmacro without-text-property (start end property &rest body)
  `(progn
     (with-silent-modifications
       (remove-text-properties (or ,start (point-min))
                               (or ,end   (point-max))
                               (list ,property nil)))
     (unwind-protect
         (prog1 (progn ,@body)
           (font-lock-flush (or ,start (point-min)) (or ,end (point-max))))
       (font-lock-flush))))

(put 'without-text-property 'lisp-indent-function 'defun)

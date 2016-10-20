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

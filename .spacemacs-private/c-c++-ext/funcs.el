(setq-default c-offset-level 1)
(defun c-set-offsets (offsets &optional level)
  (let ((level (or level c-offset-level)))
    (dolist (offset offsets)
      (let ((sym (car  offset))
            (val (cadr offset)))
        (c-set-offset sym (if (numberp val)
                              (* level val)
                            val))))))

(defun c-set-vars (vars)
  (dolist (v var)
    (let ((sym (car  v))
          (val (cadr v)))
      (set sym val))))

(defun c-setup-indent-config (config)
  (let ((var    (plist-get config 'var))
        (offset (plist-get config 'offset)))
    (when var
      (c-set-vars   var))
    (when offset
      (c-set-offsets offset))))

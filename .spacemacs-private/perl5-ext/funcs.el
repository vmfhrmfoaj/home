(setq-default perl-offset-level 1)

(defun perl-set-offsets (offsets &optional level)
  (let ((level (or level perl-offset-level)))
    (dolist (offset offsets)
      (let ((sym (car  offset))
            (val (cadr offset)))
        (set sym (if (numberp val)
                     (* level val)
                   val))))))

(defun perl-set-vars (vars)
  (dolist (v var)
    (let ((sym (car  v))
          (val (cadr v)))
      (set sym val))))

(defun perl-setup-indent-config (config)
  (let ((var (plist-get config 'var))
        (offset (plist-get config 'offset)))
    (when var
      (perl-set-vars var))
    (when offset
      (perl-set-offsets offset))))

(defvar perl-defun-regex
  "sub[ \r\t\n]+[_0-9A-Za-z]+[ \r\t\n]+{")

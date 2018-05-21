(use-package cc-mode
  :defer t
  :init
  (setq-default c-offset-level 1)

  (defun c-set-offsets (offsets &optional level)
    "TODO"
    (let ((level (or level c-offset-level)))
      (dolist (offset offsets)
        (let ((sym (car  offset))
              (val (cadr offset)))
          (c-set-offset sym (if (numberp val)
                                (* level val)
                              val))))))

  (defun c-set-vars (vars)
    "TODO"
    (dolist (v var)
      (let ((sym (car  v))
            (val (cadr v)))
        (set sym val))))

  (defun c-setup-indent-config (config)
    "TODO"
    (let ((var    (plist-get config 'var))
          (offset (plist-get config 'offset)))
      (when var
        (c-set-vars   var))
      (when offset
        (c-set-offsets offset))))

  (defun woman-at-point ()
    (interactive)
    (let ((thing (thing-at-point)))
      (woman thing)))

  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local evil-lookup-func #'woman-at-point)
              (c-toggle-auto-newline -1)
              (c-setup-indent-config c-indent-config))
            :append))

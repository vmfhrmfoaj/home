(use-package cc-mode
  :defer t
  :init
  (defvar c-indent-config--default
    '(var
      ((indent-tabs-mode nil))

      offset
      ((access-label          -1)
       (brace-list-intro      2)
       (case-label            1)
       (defun-block-intro     2)
       (inclass               2)
       (inextern-lang         0)
       (innamespace           0)
       (label                 0)
       (member-init-intro     2)
       (statement-block-intro 2)
       (statement-case-intro  1)
       (statement-case-intro  2)
       (statement-case-open   0)
       (statement-cont        2)
       (substatement          2)
       (substatement-open     0))))

  ;; TODO
  ;; Create custom `c-style'.
  (defvar c-indent-config--antlabs
    '(var
      ((indent-tabs-mode t)
       (c-special-indent-hook nil))

      offset
      ((access-label          nil)
       (arglist-close         +)
       (arglist-intro         +)
       (brace-list-intro      +)
       (brace-list-open       0)
       (case-label            0)
       (defun-block-intro     +)
       (inclass               +)
       (inextern-lang         0)
       (innamespace           0)
       (label                 nil)
       (member-init-intro     +)
       (statement-block-intro +)
       (statement-case-intro  +)
       (statement-case-intro  +)
       (statement-case-open   0)
       (statement-cont        +)
       (substatement          +)
       (substatement-open     0)
       (substatement-label    0))))

  (defcustom c-indent-config
    c-indent-config--default
    "TODO"
    :type 'list
    :safe 'listp)

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

  (defun man-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (pop-to-buffer (man (concat thing "(3)")))))

  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local evil-lookup-func #'man-at-point)
              (c-toggle-auto-newline -1)
              (c-setup-indent-config c-indent-config))
            :append))

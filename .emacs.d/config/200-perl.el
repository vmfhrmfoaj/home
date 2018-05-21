(use-package cperl-mode
  :defer t
  :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
  :init
  (defvar perl-indent-config--default
    '(var
      ((indent-tabs-mode nil))

      offset
      ((cperl-indent-level 2)
       (cperl-brace-offset 0)
       (cperl-continued-brace-offset -2)
       (cperl-label-offset -2)
       (cperl-continued-statement-offset 2)
       (cperl-extra-newline-before-brace nil)
       (cperl-extra-newline-before-brace-multiline nil)
       (cperl-merge-trailing-else t)))
    "TODO")

  (defcustom perl-indent-config
    perl-indent-config--default
    "TODO"
    :type 'list
    :safe 'listp)

  (setq-default perl-offset-level 1)

  (defun perl-set-offsets (offsets &optional level)
    "TODO"
    (let ((level (or level perl-offset-level)))
      (dolist (offset offsets)
        (let ((sym (car  offset))
              (val (cadr offset)))
          (set sym (if (numberp val)
                       (* level val)
                     val))))))

  (defun perl-set-vars (vars)
    "TODO"
    (dolist (v var)
      (let ((sym (car  v))
            (val (cadr v)))
        (set sym val))))

  (defun perl-setup-indent-config (config)
    "TODO"
    (let ((var (plist-get config 'var))
          (offset (plist-get config 'offset)))
      (when var
        (perl-set-vars var))
      (when offset
        (perl-set-offsets offset))))

  (defun cperl-beginning-of-defun ()
    "TODO"
    (let* ((cur-pos (point))
           (semi-fn-regex "sub[ \r\t\n]+[_0-9A-Za-z]+\\([ \r\t\n]*([@$%]*)\\)?")
           (fn-regex (concat semi-fn-regex "[ \r\t\n]+{"))
           (from-beg-of-deufn? (eq 'beginning-of-defun this-command))
           (cur-line-str (buffer-substring (line-beginning-position)
                                           (line-end-position))))
      (when (or from-beg-of-deufn?
                (not (string-match-p semi-fn-regex cur-line-str)))
        (end-of-line)
        (unless (re-search-backward fn-regex nil t)
          (goto-char cur-pos)
          (if from-beg-of-deufn?
              (error "Not found starting of the subroutine.")
            (while (ignore-errors (backward-up-list nil t))))))))

  (defun cperl-end-of-defun ()
    "TODO"
    (and (re-search-forward "{")
         (up-list)))

  :config
  (setq cperl-break-one-line-blocks-when-indent nil
        cperl-fix-hanging-brace-when-indent nil
        cperl-indent-region-fix-constructs nil
        cperl-indent-wrt-brace nil
        cperl-merge-trailing-else nil)
  (let ((f (byte-compile (lambda (&rest _) "Do nothing" nil))))
    (advice-add #'cperl-electric-keyword :override f)
    (advice-add #'cperl-electric-else    :override f)
    (advice-add #'cperl-electric-pod     :override f))
  (add-hook 'cperl-mode-hook
            (lambda ()
              (perl-setup-indent-config perl-indent-config)
              (setq-local beginning-of-defun-function #'cperl-beginning-of-defun)
              (setq-local end-of-defun-function #'cperl-end-of-defun))))

(use-package perl5db-as-repl
  :after cperl-mode
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'perl5db-as-repl nil 'noerror)
    (quelpa '(perl5db-as-repl :repo "vmfhrmfoaj/perl5db-as-repl" :fetcher gitlab))))

(use-package elisp-mode
  :defer t
  :config
  (defun emacs-lisp-REPL-buffer ()
    "TODO"
    (interactive)
    (pop-to-buffer (get-buffer-create "*emacs-lisp REPL*"))
    (lisp-interaction-mode))

  (defun emacs-lisp-REPL-eval-print-this-sexp ()
    "TODO"
    (interactive)
    (while (ignore-errors (backward-up-list) t))
    (when (looking-at-p "\\s-*(")
      (forward-sexp))
    (newline)
    (let ((pos (point))
          (standard-output (current-buffer)))
      (eval-last-sexp t)
      (goto-char pos)
      (insert ";;=> ")))

  (byte-compile #'emacs-lisp-REPL-buffer)
  (byte-compile #'emacs-lisp-REPL-eval-print-this-sexp)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local evil-lookup-func
                          (byte-compile
                           (lambda ()
                             (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
                             (pop-to-buffer (get-buffer "*Help*"))))))))

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

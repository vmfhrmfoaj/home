(use-package elisp-mode
  :defer t
  :init
  (defun emacs-lisp-REPL-buffer ()
    "TODO"
    (interactive)
    (pop-to-buffer (get-buffer-create "*emacs-lisp REPL*"))
    (unless (eq 'lisp-interaction-mode major-mode)
      (lisp-interaction-mode)))

  (defun emacs-lisp-REPL-eval-print-this-sexp ()
    "TODO"
    (interactive)
    (while (ignore-errors (backward-up-list) t))
    (if (looking-at-p "\\s-*(")
        (forward-sexp)
      (end-of-line))
    (newline)
    (let ((pos (point))
          ;; TODO
          ;;  pretty print
          (standard-output (current-buffer)))
      (eval-last-sexp t)
      (goto-char pos)
      (insert ";;=> ")))

  (defun emacs-lisp-evil-lookup-func ()
    "TODO"
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (-when-let (buf (->> (window-list)
                         (-map #'window-buffer)
                         (--first (string-equal "*Help*" (buffer-name it)))))
      (pop-to-buffer buf)))

  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'emacs-lisp-evil-lookup-func)
              (setq-local font-lock-multiline t)
              (eldoc-mode 1))))

(use-package elisp-slime-nav
  :ensure t
  :after emacs-lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

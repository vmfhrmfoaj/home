;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'elisp-mode nil t)
  (require 'elisp-slime-nav nil t))

(use-package elisp-mode
  :config
  (defun emacs-lisp-REPL-buffer ()
    (interactive)
    (pop-to-buffer (get-buffer-create "*emacs-lisp REPL*"))
    (unless (eq 'lisp-interaction-mode major-mode)
      (lisp-interaction-mode)))

  (defun emacs-lisp-REPL-eval-print-this-sexp ()
    (interactive)
    (while (ignore-errors (backward-up-list 1 t) t))
    (if (not (looking-at-p "\\s-*("))
        (end-of-line)
      (forward-sexp)
      (newline))
    (let ((pos (point))
          ;; TODO
          ;;  pretty print
          (standard-output (current-buffer)))
      (eval-last-sexp t)
      (newline)
      (goto-char pos)))

  (defun emacs-lisp-evil-lookup-func ()
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (-when-let (buf (->> (window-list)
                         (-map #'window-buffer)
                         (--first (string-equal "*Help*" (buffer-name it)))))
      (pop-to-buffer buf)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'emacs-lisp-evil-lookup-func)
              (setq-local font-lock-multiline t)
              (eldoc-mode 1)
              (let ((f (lambda ()
                         (when eldoc-mode
                           (run-at-time 0.01 nil #'eldoc-refresh)))))
                (add-hook 'evil-insert-state-entry-hook f nil t)
                (add-hook 'evil-insert-state-exit-hook  f nil t)))))

(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

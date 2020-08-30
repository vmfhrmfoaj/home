;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package elisp-mode
  :config
  (defun emacs-lisp-REPL-buffer ()
    "TODO"
    (interactive)
    (pop-to-buffer (get-buffer-create "*emacs-lisp REPL*"))
    (unless (eq 'lisp-interaction-mode major-mode)
      (lisp-interaction-mode)))

  (defun emacs-lisp-REPL-eval-print-this-sexp ()
    "TODO"
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
    "TODO"
    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
    (-when-let (buf (->> (window-list)
                         (-map #'window-buffer)
                         (--first (string-equal "*Help*" (buffer-name it)))))
      (pop-to-buffer buf)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'emacs-lisp-evil-lookup-func)
              (setq-local font-lock-multiline t)
              (eldoc-mode 1))))

(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

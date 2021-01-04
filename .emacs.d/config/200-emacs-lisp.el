;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package elisp-mode
  :config
  (defun emacs-lisp-REPL-buffer ()
    (interactive)
    (get-scratch-buffer-create))

  (defun emacs-lisp-REPL-switch-back ()
    (interactive)
    (->> (or (and (fboundp 'projectile-project-buffers)
                  (projectile-project-buffers))
             (buffer-list))
         (--filter (with-current-buffer it (derived-mode-p 'emacs-lisp-mode)))
         (switch-to-previous-buffer-in)))

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
              (eldoc-mode 1))))

(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

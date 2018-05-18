(use-package elisp-mode
  :defer t
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

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'elisp--binding-form-point)
              (setq-local elisp-fake-match-4 (-repeat 4 (point-min-marker)))
              (setq-local font-lock-multiline t)
              (setq-local evil-lookup-func
                          (byte-compile
                           (lambda ()
                             (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
                             (pop-to-buffer (get-buffer "*Help*")))))))

  (defface lisp-local-binding-variable-name-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used to font-lock Lisp local binding variable name.")

  (font-lock-add-keywords
   'emacs-lisp-mode
   (let* ((symbol "[-+*/=>$&?:_0-9a-zA-Z]+")
          (whitespace "[ \r\t\n]")
          (whitespace+ (concat whitespace "+"))
          (whitespace* (concat whitespace "*")))
     `(("\\s(\\(\\(?:-as\\|-some\\)?->>?\\|and\\|or\\)\\_>"
        1 'default nil)
       ("\\(?:\\s-+\\|\\s(\\)\\<\\(nil\\|t\\)\\>"
        1 'font-lock-constant-face)
       ("(\\(assert\\)"
        1 'font-lock-warning-face)
       ("\\s-\\(\\?[A-Za-z]\\)\\>"
        1 'font-lock-string-face)
       ;; local variables
       (,(concat "(\\(lexical-\\)?let\\*?" whitespace+ "(")
        (,(byte-compile
           (-partial
            (lambda (symbol+whitespace limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (comment-forward (point-max))
                (let ((local-limit (save-excursion (forward-sexp) (point))))
                  (unless (and (re-search-forward symbol+whitespace (min local-limit limit) t)
                               (ignore-errors (forward-sexp) t))
                    (set-match-data elisp-fake-match-4))
                  (goto-char local-limit))
                t))
            (concat "(\\(" symbol "\\)" whitespace+)))
         (save-excursion
           (if (in-comment?)
               (setq font-lock--skip t)
             (setq font-lock--skip nil)
             (setq elisp--binding-form-point (point))
             (safe-up-list-1)
             (point)))
         (if font-lock--skip
             (end-of-line)
           (goto-char elisp--binding-form-point))
         (1 'lisp-local-binding-variable-name-face)))
       ;; function arguments
       (,(concat "\\(defun\\|lambda\\)" whitespace+ "\\(" symbol whitespace+ "\\)?(")
        (,(byte-compile
           (-partial
            (lambda (symbol limit)
              (ignore-errors
                (when font-lock--skip
                  (error ""))
                (when (re-search-forward symbol limit t)
                  (when (string-match-p "^&" (match-string 1))
                    (set-match-data elisp-fake-match-4))
                  t)))
            (concat "\\(" symbol "\\)\\>")))
         (save-excursion
           (if (in-comment?)
               (setq font-lock--skip t)
             (setq font-lock--skip nil)
             (setq elisp--binding-form-point (point))
             (safe-up-list-1)
             (point)))
         (if font-lock--skip
             (end-of-line)
           (goto-char elisp--binding-form-point))
         (1 'lisp-local-binding-variable-name-face)))
       (,(concat "(-\\(?:when\\|if\\)-let\\*?" whitespace+ "(\\(" symbol "\\)" whitespace)
        (1 'lisp-local-binding-variable-name-face))))))

(use-package elisp-slime-nav
  :ensure t
  :after emacs-lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

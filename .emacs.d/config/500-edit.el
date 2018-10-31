(use-package aggressive-indent
  :ensure t
  :init
  (defun aggressive-indent-do-indent ()
    "TODO"
    (interactive)
    (when aggressive-indent-mode
      (save-excursion
        (save-selected-window
          (let (aggressive-indent-dont-indent-if
                aggressive-indent-protected-current-commands)
            (aggressive-indent--proccess-changed-list-and-indent))))))

  :config
  (setq aggressive-indent-sit-for-time 0.1)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-backward-barf-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-splice-sexp-killing-forward)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-splice-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-backward-slurp-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-forward-barf-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-convolute-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-splice-sexp-killing-backward)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-splice-sexp-killing-around)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-forward-slurp-sexp)
  (add-to-list 'aggressive-indent-protected-current-commands #'sp-wrap-sexp)
  (add-to-list 'aggressive-indent-excluded-modes 'autoconf-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)
  (add-to-list 'aggressive-indent-dont-indent-if '(not (eq 'insert evil-state)))
  (add-to-list 'aggressive-indent-dont-indent-if 'evil-insert-vcount)
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              "TODO"
              (unless evil-insert-vcount
                (aggressive-indent-do-indent))))
  (global-aggressive-indent-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :init
  (defun evil-surround-region-for-hkkb (args)
    "TODO"
    (if (> 4 (length args))
        args
      (let* ((char (nth 3 args))
             (new-char (cond
                        ((= 33554474 char) 35) ;;<S-kp-multiply> => #
                        ((= 33554479 char) 92) ;;<S-kp-divide> => \
                        ((= 33554477 char) 95) ;;<S-kp-subtract> => _
                        ((= 33554475 char) 61) ;;<S-kp-add> => =
                        )))
        (if new-char
            (-replace-at 3 new-char args)
          args))))

  (defun evil-surround-chnage-for-hkkb (args)
    (interactive (list (read-key)))
    args)

  :config
  (advice-add #'evil-surround-region :filter-args #'evil-surround-region-for-hkkb)
  (advice-add #'evil-surround-change :filter-args #'evil-surround-chnage-for-hkkb)
  (advice-add #'evil-surround-delete :filter-args #'evil-surround-chnage-for-hkkb)
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil)

(use-package smartparens-config
  :ensure smartparens
  :init
  (defun sp-wrap-sexp (&optional arg)
    "TODO"
    (interactive "P")
    (sp-wrap-with-pair "("))

  (defun sp--indent-region-without-protection (start end &optional column)
    "TODO"
    (unless (and (eq 'insert evil-state)
                 (bound-and-true-p aggressive-indent-mode))
      ;; Don't issue "Indenting region..." message.
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region start end column))))

  (defun sp-elixir-do-block-post-handler-2 (_id action _context)
    "Insert \"do\" keyword and indent the new block.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (let ((pos (point)))
        (insert "do")
        (newline)
        (save-excursion
          (newline)
          (indent-region pos (line-end-position)))
        (indent-for-tab-command))))

  (defun sp-elixir-skip-single-line-do-p (_ms mb _me)
    "TODO"
    (save-match-data
      (save-excursion
        (goto-char mb)
        (and (re-search-forward "\\_<do:" (line-end-position) t) t))))

  (defun sp-elixir-single-line-do-p (_id _action _context)
    "TODO"
    (save-match-data
      (save-excursion
        (and (re-search-forward "\\_<do:" (line-end-position) t) t))))

  (defun sp-org-checkbox-p (_id _action _context)
    "TODO"
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (and (re-search-forward "^\\s-*\\(?:-\\|[0-9]+\\.\\) \\[" (line-end-position) t) t))))

  (defun sp-org-checkbox-handler (id action context)
    (when (and (string-equal id "[")
               (eq action 'insert)
               (sp-org-checkbox-p id action context))
      (insert " ")
      (skip-chars-forward "[^[]")
      (insert " ")))

  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (advice-add #'sp--indent-region :override #'sp--indent-region-without-protection)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package whitespace
  :config
  (setq-default whitespace-line-column 120)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

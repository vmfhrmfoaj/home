(use-package aggressive-indent
  :ensure t
  :init
  (defun aggressive-indent-do-indent ()
    (interactive)
    (when aggressive-indent-mode
      (while-no-input
        (aggressive-indent--proccess-changed-list-and-indent))))

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

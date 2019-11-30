(use-package aggressive-indent
  :ensure t
  :defer t
  :config
  (defn aggressive-indent-do-indent ()
    "TODO"
    (interactive)
    (when aggressive-indent-mode
      (save-excursion
        (save-selected-window
          (let (aggressive-indent-dont-indent-if
                aggressive-indent-protected-current-commands)
            (aggressive-indent--proccess-changed-list-and-indent))))))

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
            (byte-compile
             (lambda ()
               "TODO"
               (unless evil-insert-vcount
                 (aggressive-indent-do-indent))))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (defn evil-surround-region-for-hkkb (args)
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

  (defn evil-surround-chnage-for-hkkb (args)
    (interactive (list (read-key)))
    args)

  (when HHKB?
    (advice-add #'evil-surround-region :filter-args #'evil-surround-region-for-hkkb)
    (advice-add #'evil-surround-change :filter-args #'evil-surround-chnage-for-hkkb)
    (advice-add #'evil-surround-delete :filter-args #'evil-surround-chnage-for-hkkb))

  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil)

(use-package flymake
  :defer t
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package smartparens-config
  :ensure smartparens
  :config
  (defn sp-wrap-sexp (&optional arg)
    "TODO"
    (interactive "P")
    (sp-wrap-with-pair "("))

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package whitespace
  :config
  (setq-default whitespace-line-column 120)

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

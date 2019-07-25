(with-eval-after-load "frame"
  (blink-cursor-mode 0))

(use-package aggressive-indent
  :ensure t
  :defer t
  :init
  (defn aggressive-indent-do-indent ()
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
                (aggressive-indent-do-indent)))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :init
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

  :config
  (when HHKB?
    (advice-add #'evil-surround-region :filter-args #'evil-surround-region-for-hkkb)
    (advice-add #'evil-surround-change :filter-args #'evil-surround-chnage-for-hkkb)
    (advice-add #'evil-surround-delete :filter-args #'evil-surround-chnage-for-hkkb))
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil)

(use-package flycheck
  :disabled t
  :ensure t
  :defer t
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011001
              #b00110110
              #b01101100
              #b11011000
              #b01101100
              #b00110110
              #b00011001
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))))

(use-package flymake
  :defer t
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package smartparens-config
  :ensure smartparens
  :init
  (defn sp-wrap-sexp (&optional arg)
    "TODO"
    (interactive "P")
    (sp-wrap-with-pair "("))

  (defn sp-org-checkbox-p (_id _action _context)
    "TODO"
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (and (re-search-forward "^\\s-*\\(?:-\\|[0-9]+\\.\\) \\[\\(?:\\]\\|$\\)" (line-end-position) t) t))))

  (defn sp-org-checkbox-handler (id action context)
    "TODO"
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
  (with-eval-after-load "org"
    (sp-local-pair 'org-mode "[" "]" :post-handlers '(:add sp-org-checkbox-handler)))
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package whitespace
  :config
  (setq-default whitespace-line-column 120)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

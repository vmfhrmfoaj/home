(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'autoconf-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)
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

  :config
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

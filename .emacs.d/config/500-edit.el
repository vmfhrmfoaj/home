;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package editorconfig
  :ensure t
  :hook ((autoconf-mode . editorconfig-mode-apply))
  :init
  (editorconfig-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
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

  (when HHKB?
    (advice-add #'evil-surround-region :filter-args #'evil-surround-region-for-hkkb)
    (advice-add #'evil-surround-change :filter-args #'evil-surround-chnage-for-hkkb)
    (advice-add #'evil-surround-delete :filter-args #'evil-surround-chnage-for-hkkb))

  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil)

(use-package smartparens-config
  :ensure smartparens
  :config
  (defun sp-wrap-sexp (&optional arg)
    "TODO"
    (interactive "P")
    (sp-wrap-with-pair "("))

  (defun sp--simulate-evil-jump-item ()
    "Sometimes `evil-jump-item' does not jump to the correct position.
So, replaced `evil-jump-item' to this function."
    (interactive)
    (-let (((beg end beg-len end-len) sp-show-pair-previous-match-positions)
           (pos (point)))
      (cond
       ((eq pos beg)
        (goto-char end)
        (forward-char (- end-len)))
       ((eq pos end)
        (goto-char end))
       (t
        (sp-backward-up-sexp)))))

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  (ignore-errors
    (ad-remove-advice #'company--insert-candidate 'after 'sp-company--insert-candidate)))

(use-package undo-tree
  :ensure t
  :defer t
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist backup-directory-alist))

(use-package whitespace
  :config
  (setq-default whitespace-line-column 120)

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

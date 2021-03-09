;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package aggressive-indent
  :ensure t
  :defer t
  :hook ((cider-repl-mode       . aggressive-indent-mode)
         (clojure-mode          . aggressive-indent-mode)
         (clojurec-mode         . aggressive-indent-mode)
         (clojurescript-mode    . aggressive-indent-mode)
         (emacs-lisp-mode       . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode))

  :config
  (setq aggressive-indent-region-function #'indent-region ; fix with below advice func
        aggressive-indent-protected-commands (->> aggressive-indent-protected-commands
                                                  (append '(evil-undo
                                                            evil-redo
                                                            ;; sp-backward-barf-sexp
                                                            ;; sp-splice-sexp-killing-forward
                                                            ;; sp-splice-sexp
                                                            ;; sp-backward-slurp-sexp
                                                            ;; sp-forward-barf-sexp
                                                            sp-convolute-sexp
                                                            ;; sp-splice-sexp-killing-backward
                                                            ;; sp-splice-sexp-killing-around
                                                            ;; sp-forward-slurp-sexp
                                                            ;; sp-wrap-sexp
                                                            undo-tree-visualize-undo
                                                            undo-tree-visualize-redo))
                                                  (-distinct)))

  (add-hook 'aggressive-indent-mode-hook
            (lambda ()
              (when (and (derived-mode-p 'clojure-mode) (featurep 'cider))
                (setq cider-dynamic-indentation nil))))

  (advice-add #'aggressive-indent--keep-track-of-changes
              :before-until
              (lambda (l r &rest _)
                "Prevent to indent changed region when inserting REPL output on `cider-repl-mode'."
                (when (and (derived-mode-p 'cider-repl-mode)
                           (< l (marker-position cider-repl-input-start-mark)))
                  t)))

  (advice-add #'indent-region :around
              (lambda (fn &rest args)
                "Wrap to hide 'Intending region... done' message"
                (if aggressive-indent-mode
                    (cl-letf (((symbol-function 'make-progress-reporter) #'ignore)
                              ((symbol-function 'message) #'ignore))
                      (apply fn args))
                  (apply fn args)))))

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
  :after evil
  :config
  (let ((fn (lambda ()
              (iedit-update-index (point))
              (force-mode-line-update))))
    (advice-add #'evil-multiedit-next :after fn)
    (advice-add #'evil-multiedit-prev :after fn)))

(use-package smartparens
  :ensure t
  :defer t
  :init
  (defun setup-smartparens-once ()
    (when (and (member this-command '(evil-insert self-insert-command))
               (not (or buffer-read-only
                        view-mode)))
      (remove-hook 'pre-command-hook #'setup-smartparens-once)
      (smartparens-global-mode 1)
      (show-smartparens-global-mode 1)
      (fmakunbound #'setup-smartparens-once)))

  (add-hook 'pre-command-hook #'setup-smartparens-once)

  :config
  (defun sp-wrap-sexp (&optional arg)
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
        sp-highlight-wrap-tag-overlay nil))

(use-package smartparens-config
  :ensure smartparens
  :after smartparens
  :config
  (defun sp--org-checkbox-p (_id _action _context)
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (and (re-search-forward "^\\s-*\\(?:-\\|[0-9]+\\.\\) \\[\\(?:\\]\\|$\\)" (line-end-position) t) t))))

  (defun sp--org-checkbox-handler (id action context)
    (when (and (string-equal id "[")
               (eq action 'insert)
               (sp--org-checkbox-p id action context))
      (insert " ")
      (skip-chars-forward "[^[]")
      (insert " ")))

  (sp-local-pair 'org-mode "[" "]" :post-handlers '(:add sp--org-checkbox-handler)))

(use-package undo-tree
  :ensure t
  :config
  ;; NOTE
  ;;  `goto-chr' require `undo-tree-node-p' function, but it is macro in `undo-tree'.
  (defun undo-tree-node-p (n)
    (let ((len (length (undo-tree-make-node nil nil))))
      (and (vectorp n) (= (length n) len))))

  (setq evil-undo-system 'undo-tree
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist backup-directory-alist)

  (with-eval-after-load "evil"
    (evil-set-undo-system 'undo-tree))

  (global-undo-tree-mode)

  ;; NOTE
  ;;  `undo-tree-save-history-from-hook' takes more than 1 second.
  ;;  It annoy me very much.
  (remove-hook 'write-file-functions #'undo-tree-save-history-from-hook)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (->> (buffer-list)
                   (--filter buffer-file-name)
                   (--map (kill-buffer it))))))

(use-package whitespace
  :config
  (setq-default whitespace-line-column 120)

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

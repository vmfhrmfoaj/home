;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package aggressive-indent
  :ensure t
  :defer t
  :hook ((cider-repl-mode       . aggressive-indent-mode)
         (clojure-mode          . aggressive-indent-mode)
         (clojurec-mode         . aggressive-indent-mode)
         (clojurescript-mode    . aggressive-indent-mode)
         (emacs-lisp-mode       . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode))
  :init
  (eval-when-compile (require 'aggressive-indent nil t))

  :config
  (setq aggressive-indent-sit-for-time 0.01
        aggressive-indent-region-function #'indent-region ; fix with below advice func
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

  ;; (add-hook 'aggressive-indent-mode-hook
  ;;           (lambda ()
  ;;             (when (and (derived-mode-p 'clojure-mode) (featurep 'cider))
  ;;               (setq cider-dynamic-indentation nil))))

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
  (eval-when-compile (require 'editorconfig nil t))

  (editorconfig-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :init
  (eval-when-compile (require 'evil-surround nil t))

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

  (advice-add #'evil-surround-region :filter-args #'evil-surround-region-for-hkkb)
  (advice-add #'evil-surround-change :filter-args #'evil-surround-chnage-for-hkkb)
  (advice-add #'evil-surround-delete :filter-args #'evil-surround-chnage-for-hkkb)

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

(use-package iedit
  :defer t
  :init
  (eval-when-compile (require 'iedit nil t))

  :config
  (setq iedit-mode-line '(" Iedit[" (:eval (format "%d/%d" iedit-occurrence-index (iedit-counter))) "]"))
  (-update->> minor-mode-alist
              (--remove (eq 'iedit-mode (car it)))
              (-concat `((iedit-mode ,iedit-mode-line)))))

(use-package smartparens
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'smartparens nil t))

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

  (sp-local-pair 'org-mode "[" "]" :post-handlers '(:add sp--org-checkbox-handler))

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package smartparens-config
  :ensure smartparens
  :init
  (eval-when-compile (require 'smartparens-config nil t)))

(use-package undo-tree
  :ensure t
  :init
  (eval-when-compile (require 'undo-tree nil t))

  :config
  ;; NOTE
  ;;  `goto-chr' require `undo-tree-node-p' function, but it is macro in `undo-tree'.
  (defun undo-tree-node-p (n)
    (let ((len (length (undo-tree-make-node nil nil))))
      (and (vectorp n) (= (length n) len))))

  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `((".*" . ,(concat home-dir "/.emacs.d/undo-tree/"))))

  (with-eval-after-load "evil"
    (evil-set-undo-system 'undo-tree))

  (add-hook 'find-file-hook
            (lambda ()
              "Enable `undo-tree-mode' if a file is unstaged."
	          (when (fboundp 'magit-unstaged-files)
                (let* ((file (buffer-file-name))
                       (file-name (file-name-nondirectory file)))
                  (when (null (magit-unstaged-files nil file-name))
                    (delete-file (undo-tree-make-history-save-file-name (buffer-file-name)))
                    (setq buffer-undo-tree nil)))))
            -100)

  (add-hook 'after-save-hook
            (lambda ()
              "Enable/disable `undo-tree-mode'."
	          (when (fboundp 'magit-unstaged-files)
                (when-let ((file (buffer-file-name)))
                  (let ((file-name (file-name-nondirectory file)))
                    (when (null (magit-unstaged-files nil file-name))
                      (delete-file (undo-tree-make-history-save-file-name (buffer-file-name))))))))
            100)

  (with-eval-after-load "magit"
    (add-hook 'magit-post-refresh-hook
              (lambda ()
                "Enable/disable `undo-tree-mode'."
                (let* ((unstaged-files (--map (concat (magit-rev-parse-safe "--show-toplevel") "/" it)
                                              (magit-unstaged-files)))
                       (all-bufs (buffer-list))
                       (undo-tree-bufs (--filter (with-current-buffer it undo-tree-mode) all-bufs))
                       (unstaged-bufs  (--filter (let ((file (buffer-file-name it)))
                                                   (--some (string-equal file it) unstaged-files))
                                                 all-bufs)))
                  (--each undo-tree-bufs
                    (when-let ((file (buffer-file-name it)))
                      (unless (memq it unstaged-bufs)
                        (delete-file (undo-tree-make-history-save-file-name file)))))))))

  (advice-add #'undo-tree-save-history-from-hook :before-while
              (lambda (&rest _)
	            (when (fboundp 'magit-unstaged-files)
                  (when-let ((file (buffer-file-name)))
                    (let ((file-name (file-name-nondirectory file)))
                      (magit-unstaged-files nil file-name))))))

  (global-undo-tree-mode 1)

  ;; NOTE
  ;;  `undo-tree-save-history-from-hook' takes more than 1 second.
  ;;  It annoy me very much.
  (remove-hook 'write-file-functions #'undo-tree-save-history-from-hook)
  (add-hook 'kill-emacs-hook ; trigger `undo-tree-save-history-from-hook'
            (lambda ()
              (->> (buffer-list)
                   (--filter buffer-file-name)
                   (--map (kill-buffer it))))))

(use-package whitespace
  :init
  (eval-when-compile (require 'whitespace nil t))

  :config
  (setq-default whitespace-line-column 120)

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace t))))

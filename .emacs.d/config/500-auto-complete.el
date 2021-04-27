;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode-on)
         (eshell-mode . company-mode-on))
  :init
  (eval-when-compile (require 'company nil t))

  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (setq company-dabbrev-code-ignore-case t
        company-dabbrev-code-other-buffers nil
        company-etags-ignore-case t
        company-idle-delay nil
        company-echo-delay nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-flip-when-above t)

  (add-hook 'company-mode-hook
            (lambda ()
              ;; NOTE
              ;;  use `counsel-company' instead of `company-tooltip'.
              (remove-hook 'pre-command-hook  'company-pre-command t)
              (remove-hook 'post-command-hook 'company-post-command t)
              (remove-hook 'yas-keymap-disable-hook 'company--active-p t)))

  (add-hook 'company-after-completion-hook
            (lambda (_ignored)
              (when (timerp eldoc-timer)
                (cancel-timer eldoc-timer)
                (setq eldoc-timer nil))
              (run-at-time 0.1 nil (-partial #'call-interactively #'eldoc-refresh))))

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (company--active-p)
                (company-cancel))))

  (add-hook 'evil-local-mode-hook
            (lambda ()
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist))))

  (advice-add #'company--insert-candidate :filter-args
              (lambda (args)
                "Remove the whitespace"
                (if-let ((candidate (car args)))
                    (list (string-trim candidate))
                  (list ""))))

  (with-eval-after-load "lsp-mode"
    (advice-add #'company-echo-show :around
                (lambda (fn &rest args)
                  "To disable `company-echo' when `lsp-signature-mode' is being enabled."
                  (unless (bound-and-true-p lsp-signature-mode)
                    (apply fn args))))))

(use-package counsel
  :defer t
  :init
  (eval-when-compile (require 'counsel nil t))

  (defface counsel-company-annotation-face
    '((t :inherit shadow))
    "TODO")

  :config
  (defun counsel--company-display-transformer (s)
    (concat s (let ((annot (company-call-backend 'annotation s)))
                (when annot
                  (propertize " " 'face 'counsel-company-annotation-face 'display (company--clean-string annot))))))

  (defun counsel--custom-company ()
    "Custom `counsel-company'"
    (interactive)
    (company-mode 1)
    (unless company-candidates
      (company-complete))
    (if company-candidates
        (let ((len (cond ((let (l)
                            (and company-common
                                 (string= company-common
                                          (buffer-substring
                                           (- (point) (setq l (length company-common)))
                                           (point)))
                                 l)))
                         (company-prefix
                          (length company-prefix)))))
          (when len
            (setq ivy-completion-beg (- (point) len))
            (setq ivy-completion-end (point))
            (ivy-read "Candidate: " company-candidates
                      :action #'ivy-completion-in-region-action
                      :caller 'counsel-company
                      :initial-input (concat (downcase (or company-common company-prefix)) " ")
                      :sort t)))
      (message "There is no candidate.")))

  (-update->> ivy--display-transformers-alist
              (--remove (-let (((caller . _rest) it))
                          (eq 'counsel-company caller))))

  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-company . ivy--shorter-matches-first))
  (add-to-list 'ivy--display-transformers-alist '(counsel-company . counsel--company-display-transformer))

  (advice-add #'counsel-company :override #'counsel--custom-company)

  (with-eval-after-load "company"
    (advice-add #'company-indent-or-complete-common :override
                (lambda (arg)
                  "Customize `company-indent-or-complete-common' use `counsel-company' instead of `company' overlay."
                  (interactive "P")
                  (cond
                   ((use-region-p)
                    (indent-region (region-beginning) (region-end)))
                   ((memq indent-line-function
                          '(indent-relative indent-relative-maybe))
                    (counsel-company))
                   ((let ((old-point (point))
                          (old-tick (buffer-chars-modified-tick))
                          (tab-always-indent t))
                      (indent-for-tab-command arg)
                      (when (and (eq old-point (point))
                                 (eq old-tick (buffer-chars-modified-tick)))
                        (counsel-company)))))))))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode-on)
  :init
  (eval-when-compile (require 'yasnippet nil t))

  :config
  (with-eval-after-load "lsp-mode"
    (add-hook 'yas-before-expand-snippet-hook
              (lambda ()
                "Enable `lsp-signature-mode'"
                (when (and lsp-mode
                           (lsp-feature? "textDocument/signatureHelp")
                           (null lsp-signature-mode))
                  (ignore-errors
                    (setq lsp-signature-restart-enable t)
                    (lsp-signature-activate)))))))

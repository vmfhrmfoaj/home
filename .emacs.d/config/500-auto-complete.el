(use-package company
  :ensure t
  :hook (prog-mode . company-mode-on)
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (setq company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-etags-ignore-case t))

(use-package helm-company
  :ensure t
  :after (company helm)
  :config
  (defn helm-company-plus ()
    "TODO"
    (interactive)
    (unless company-candidates
      (company-complete)
      ;; (company-call-frontends 'hide) Work around a quirk with company.
      ;; `company-complete' inserts the common part of all candidates into the
      ;; buffer. But, it doesn't update `company-prefix' -- and `company-prefix'
      ;; is all `company-finish' replaces in the buffer. (issue #9)
      (when company-common
        (setq company-prefix company-common)))
    (let ((initial-pattern (and helm-company-initialize-pattern-with-prefix company-prefix)))
      (when company-point
        (helm :sources 'helm-source-company
              :buffer  "*helm company*"
              :input (unless (s-blank-str? company-prefix)
                       (-> company-prefix
                           (regexp-quote)
                           (concat " ")
                           (propertize 'rear-nonsticky '(read-only intangible)
                                       'read-only t
                                       'intangible t)))
              :candidate-number-limit helm-company-candidate-number-limit)
        (when (or (not (featurep 'yasnippet))
                  (null yas--active-snippets)
                  (not (looking-back "\\s(")))
          (evil-normal-state))
        (when (and (featurep 'lsp)
                   lsp-signature-auto-activate
                   (lsp-feature? "textDocument/signatureHelp"))
          (lsp-signature-activate)))))

  (defn helm-company-complete-common ()
    "TODO"
    (interactive)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (helm-company-plus)
      (company--insert-candidate company-common)))

  (defn company-indent-or-helm-company ()
    "TODO"
    (interactive)
    (cond
     ((use-region-p)
      (indent-region (region-beginning) (region-end)))
     ((memq indent-line-function '(indent-relative indent-relative-maybe))
      (helm-company-complete-common))
     ((let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (tab-always-indent t))
        (call-interactively #'indent-for-tab-command)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (helm-company-complete-common))))))

  (add-hook 'helm-cleanup-hook
            (byte-compile
             (lambda ()
               (with-helm-current-buffer
                 (unless (minibufferp)
                   (company-abort))))))
  ;; NOTE
  ;;  Turn company popup off completely.
  (add-hook 'company-mode-hook
            (lambda ()
              "Prevent raising a popup box."
              (remove-hook 'pre-command-hook 'company-pre-command t)
              (remove-hook 'post-command-hook 'company-post-command t))))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode-on))

;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode-on)
         (eshell-mode . company-mode-on))
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (setq company-backends (->> company-backends
                              (--map (if (listp it)
                                         (--remove (or (eq 'company-dabbrev it)
                                                       (eq 'company-dabbrev-code it))
                                                   it)
                                       (unless (or (eq 'company-dabbrev it)
                                                   (eq 'company-dabbrev-code it))
                                         it)))
                              (-non-nil))
        company-idle-delay 0
        company-echo-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-etags-ignore-case t
        company-tooltip-flip-when-above t)

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
  (advice-add #'company-echo-show :around
              (lambda (fn &rest args)
                "To disable `company-echo' when `lsp-signature-mode' is being enabled."
                (unless (bound-and-true-p lsp-signature-mode)
                  (apply fn args)))))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode-on)
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

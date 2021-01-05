;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode-on)
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (let ((len (if (fboundp #'native-compile) 1 3))
        (idle-delay (if (fboundp #'native-compile) 0.15 0.3)))
    (setq company-backends (--remove (eq 'company-dabbrev it) company-backends)
          company-idle-delay idle-delay
          company-echo-delay 0.2
          company-minimum-prefix-length len
          company-selection-wrap-around t
          company-dabbrev-code-ignore-case t
          company-etags-ignore-case t
          company-tooltip-flip-when-above t))

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

  ;; NOTE
  ;;  sometimes all candidates can be prefixed with spaces.
  (advice-add #'company--insert-candidate :filter-args
              (-compose #'-list (-partial #'s-chop-prefix " ") #'car)))

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

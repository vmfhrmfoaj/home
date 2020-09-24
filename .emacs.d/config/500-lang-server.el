;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package dap-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package lsp-clients
  :defer t
  :config
  (add-to-list 'lsp-clients-clangd-args "--header-insertion=never")

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                    :major-modes '(sh-mode)
                    :priority -1
                    :environment-fn (lambda ()
                                      (let ((env nil))
                                        (when lsp-bash-explainshell-endpoint
                                          (-update-> env (append '(("EXPLAINSHELL_ENDPOINT" .
                                                                    lsp-bash-explainshell-endpoint)))))
                                        (when lsp-bash-highlight-parsing-errors
                                          (-update-> env (append '(("HIGHLIGHT_PARSING_ERRORS" .
                                                                    lsp-bash-highlight-parsing-errors)))))
                                        env))
                    :server-id 'bash-ls)))

(use-package lsp-intelephense
  :defer t
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (-const lsp-intelephense-server-command))
                    :major-modes '(php-mode)
                    :priority -1
                    :notification-handlers (ht ("indexingStarted" #'ignore)
                                               ("indexingEnded" #'ignore))
                    :initialization-options (lambda ()
                                              (list :globalStoragePath lsp-intelephense-storage-path
                                                    :storagePath lsp-intelephense-storage-path
                                                    :licenceKey lsp-intelephense-licence-key
                                                    :clearCache lsp-intelephense-clear-cache))
                    :multi-root lsp-intelephense-multi-root
                    :completion-in-comments? t
                    :server-id 'iph)))

(use-package lsp-java
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode          . lsp)
         (c++-mode        . lsp)
         (cperl-mode      . lsp)
         (go-mode         . lsp)
         (js-mode         . lsp)
         (latex-mode      . lsp)
         (php-mode        . lsp)
         (rust-mode       . lsp)
         (sh-mode         . lsp)
         (typescript-mode . lsp))
  :init
  (setq lsp-keymap-prefix nil)

  :config
  (defun lsp--wrap-find-xxx (f &rest args)
    "Fall back to `dumb-jump-go'."
    (let ((pos (point))
          (cur-buf (current-buffer)))
      (apply f args)
      (when (and (eq cur-buf (current-buffer))
                 (eq pos (point)))
        (ignore-errors (ring-remove xref--marker-ring 0))
        (message nil)
        (call-interactively #'dumb-jump-go))))

  (defun lps--focus-lsp-help-buffer (&rest _)
    "Force on '*lsp-help*' buffer"
    (-when-let (buf (get-buffer "*lsp-help*"))
      (pop-to-buffer buf)))

  (defun lsp--custom-eldoc-message (&optional msg)
    "Show MSG in eldoc."
    (let ((pos (eldoc-refresh-pos)))
      (when (and eldoc-refresh-last-pos
                 (not (equal eldoc-refresh-last-pos pos)))
        (setq eldoc-refresh-last-pos pos
              lsp--eldoc-saved-message msg)
        (let ((lines (s-lines (or msg "")))
              (max-line (cond
                         ((floatp max-mini-window-height)
                          (ceiling (* (frame-height) max-mini-window-height)))
                         ((numberp max-mini-window-height)
                          max-mini-window-height)
                         (t 10))))
          (eldoc-message (when lines
                           (->> (if (<= (length lines) max-line)
                                    lines
                                  (-snoc (-take (max 1 (1- max-line)) lines) (propertize "(...)" 'face 'shadow)))
                                (-interpose "\n")
                                (apply #'concat))))))))

  (setq lsp-diagnostic-package :flycheck
        lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-file-watch-threshold nil
        lsp-idle-delay 0.2
        lsp-restart 'ignore
        lsp-rust-server 'rust-analyzer)

  (setq-default lsp-eldoc-enable-hover t
                lsp-enable-on-type-formatting nil)

  (add-to-list 'lsp-language-id-configuration '(cperl-mode . "perl"))
  (add-to-list 'lsp-language-id-configuration '(".*\\.pl$" . "perl"))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (cond
               ((eq major-mode 'go-mode)
                (setq-local lsp-eldoc-render-all t)))
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          (when lsp-mode
                            (when (and lsp-signature-auto-activate
                                       (lsp-feature? "textDocument/signatureHelp")
                                       (null lsp-signature-mode))
                              (setq-local lsp-eldoc-enable-hover nil)
                              (lsp-signature-activate))
                            (when lsp-ui-sideline-mode
                              (lsp-ui-sideline-mode -1))))
                        nil t)
              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          (when lsp-mode
                            (when lsp-signature-auto-activate
                              (setq lsp-eldoc-enable-hover (default-value 'lsp-eldoc-enable-hover))
                              (lsp-signature-stop))
                            (unless lsp-ui-sideline-mode
                              (lsp-ui-sideline-mode 1))))
                        nil t)
              (add-hook 'evil-operator-state-entry-hook
                        (lambda ()
                          (when (and lsp-mode
                                     lsp-ui-sideline-mode)
                            (lsp-ui-sideline--delete-ov)
                            (setq lsp-ui-sideline--tag nil)))
                        nil t)))
  (add-hook 'lsp-after-diagnostics-hook
            (lambda ()
              (when (and lsp-ui-sideline-mode (not (buffer-modified-p)))
                (lsp-ui-sideline--diagnostics-changed))))

  (advice-add #'lsp--eldoc-message :override #'lsp--custom-eldoc-message)
  (advice-add #'lsp-describe-thing-at-point :after #'lps--focus-lsp-help-buffer)

  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx))

(use-package lsp-perl
  :defer t
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tcp-connection
                                     (lambda (port)
                                       (list lsp-perl-language-server-path
                                             "-MPerl::LanguageServer"
                                             "-ePerl::LanguageServer::run"
                                             "--"
                                             "--port"
                                             (number-to-string port))))
                    :major-modes '(perl-mode cperl-mode)
                    :priority -1
                    :server-id 'perl-language-server)))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil))

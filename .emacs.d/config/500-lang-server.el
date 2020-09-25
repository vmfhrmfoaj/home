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

  (defvar lsp--custom-render--regex-1-for-php
    (concat "\\(?:^\\|\n\\)````php\n"              ; ```php
            "\\(?:[ \t\r\n]*<\\?php[ \t\r\n]*\\)?" ; <?php
            "\\(\\(?:\n\\|.\\)+?\\)"               ; <CONTENTS>
            "\\(?:[ \t\r\n]*{\\s-*}[ \t\r\n]*\\)?" ; { }
            "[ \t\r\n]*\\(?:[ \t\r\n]*\\?>"        ; ?>
            "[ \t\r\n]*\\)?```\\(?:\n\\|$\\)")     ; ```
    "TODO")
  (defvar lsp--custom-render--regex-2-for-php
    "\\(?:_@var_\\|_@param_\\)\\s-*`?\\s-*\\(.+?\\)\\s-*`?$"
    "TODO")

  (defvar lsp--custom-render--regex-for-rust
    (concat "^[ \t\r\n]*"
            "```rust\n"                 ; ```rust
            "\\(\\(?:\n\\|.\\)+?\\)\n"  ; <CONTENTS>
            "```")                      ; ```
    "TODO")

  (defvar lsp--custom-render--regex-1-for-shell
    "^SYNOPSIS[ \t\r]*\n[ \t\r]*\\(\\(?:.\\|[\r\n]+\\)+?\\)[ \t\r]*\n[ \t\r]*\n"
    "TODO")
  (defvar lsp--custom-render--regex-2-for-shell
    "^[^ :]+?:[ \t]+\\([^\r\n]+\\)"
    "TODO")

  (defun lsp--adapter-render-on-hover-content (args)
    "TODO"
    (let ((contents (car args)))
      (cond
       ((and (derived-mode-p 'rust-mode)
             (hash-table-p contents))
        (when (string= "markdown" (or (gethash "kind" contents) ""))
          (let ((md (gethash "value" contents)))
            (when (and (string-match lsp--custom-render--regex-for-rust md)
                       (= (match-beginning 0) 0))
              (when-let ((val (match-string 1 md)))
                (when (string-match lsp--custom-render--regex-for-rust md (match-end 0))
                  (setq val (concat val "\n" (match-string 1 md))))
                (puthash "language" "rust" contents)
                (remhash "kind" contents)
                (puthash "value" val contents))))))
       ((and (derived-mode-p 'sh-mode)
             (hash-table-p contents))
        (puthash "language" "shellscript" contents)
        (when (string= "markdown" (or (gethash "kind" contents) ""))
          (let ((md (gethash "value" contents)))
            (when (or (string-match lsp--custom-render--regex-1-for-shell md)
                      (string-match lsp--custom-render--regex-2-for-shell md))
              (remhash "kind" contents)
              (puthash "value"
                       (->> md
                            (match-string 1)
                            (s-lines)
                            (-map #'s-trim)
                            (-interpose "\n")
                            (apply #'concat))
                       contents)))))
       ((and (derived-mode-p 'php-mode)
             (hash-table-p contents))
        (puthash "language" "php" contents)
        (when (string= "markdown" (or (gethash "kind" contents) ""))
          (let ((md (gethash "value" contents)))
            (when (or (string-match lsp--custom-render--regex-1-for-php md)
                      (string-match lsp--custom-render--regex-2-for-php md))
              (remhash "kind" contents)
              (puthash "value"
                       (->> md
                            (match-string 1)
                            (s-lines)
                            (-map #'s-trim)
                            (apply #'concat)
                            (s-replace "," ", "))
                       contents))))))
      (if (not (listp contents))
          (apply #'list contents (cdr args))
        (apply #'list (-interpose "\n" (append contents nil)) (cdr args)))))

  (defun lsp--sanitate-hover-saved-bounds ()
    "Clear `lsp--hover-saved-bounds' if "
    (when (and lsp--hover-saved-bounds
               (derived-mode-p 'rust-mode))
      (let ((cur (point))
            (beg (car lsp--hover-saved-bounds))
            (end (cdr lsp--hover-saved-bounds)))
        (print (buffer-substring-no-properties beg end))
        (save-excursion
          (goto-char beg)
          (when (cond
                 ((< (line-end-position) end) t)
                 ((and (string-match-p "[;?.()]" (char-to-string (char-after end)))
                       (not (string-match-p "[;?.()]" (char-to-string (char-after cur)))))))
            (setq lsp--hover-saved-bounds nil))))))

  (defun lsp--custom-eldoc-message-emacs-27 (&optional msg)
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

  (defun lsp--custom-eldoc-message-emacs-28 (&optional msg)
    "Show MSG in eldoc."
    (setq lsp--eldoc-saved-message msg)
    (eldoc-message msg))

  (defalias 'lsp--custom-eldoc-message
    (if (version<= "28.0.50" emacs-version)
        #'lsp--custom-eldoc-message-emacs-28
      #'lsp--custom-eldoc-message-emacs-27))

  (defun lsp--signature->message-filter (msg)
    (replace-in-string " │ " " | " msg))

  (setq lsp-diagnostic-package :flycheck
        lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-file-watch-threshold nil
        lsp-idle-delay 0.2
        lsp-restart 'ignore
        lsp-rust-server 'rust-analyzer
        lsp-signature-function (lambda (msg)
                                 (when lsp--on-idle-timer
                                   (cancel-timer lsp--on-idle-timer))
                                 (when eldoc-timer
                                   (cancel-timer eldoc-timer))
                                 (lsp--custom-eldoc-message msg)))
  (setq-default lsp-eldoc-enable-hover t
                lsp-enable-on-type-formatting nil)

  (add-to-list 'lsp-language-id-configuration '(cperl-mode . "perl"))
  (add-to-list 'lsp-language-id-configuration '(".*\\.pl$" . "perl"))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (cond
               ((eq major-mode 'go-mode)
                (setq-local lsp-eldoc-render-all t)
                (add-hook 'before-save-hook #'lsp-format-buffer t t)
                (add-hook 'before-save-hook #'lsp-organize-imports t t)))
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          (when lsp-mode
                            (when lsp-ui-sideline-mode
                              (lsp-ui-sideline-mode -1))
                            (when (and lsp-signature-auto-activate
                                       (lsp-feature? "textDocument/signatureHelp")
                                       (null lsp-signature-mode))
                              (setq-local lsp-eldoc-enable-hover nil)
                              (lsp-signature-activate))))
                        nil t)
              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          (unless lsp-ui-sideline-mode
                            (lsp-ui-sideline-mode 1))
                          (when lsp-mode
                            (when lsp-signature-auto-activate
                              (setq lsp-eldoc-enable-hover (default-value 'lsp-eldoc-enable-hover))
                              (lsp-signature-stop))))
                        nil t)
              (add-hook 'evil-operator-state-entry-hook
                        (lambda ()
                          (when (and lsp-mode
                                     lsp-ui-sideline-mode)
                            (lsp-ui-sideline--delete-ov)
                            (setq lsp-ui-sideline--tag nil)))
                        nil t)))

  (advice-add #'lsp--eldoc-message :override 'lsp--custom-eldoc-message)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--adapter-render-on-hover-content)
  (advice-add #'lsp--signature->message :filter-return #'lsp--signature->message-filter)
  (advice-add #'lsp-hover :before #'lsp--sanitate-hover-saved-bounds)
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

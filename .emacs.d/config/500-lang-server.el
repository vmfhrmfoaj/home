;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package dap-mode
  :ensure t
  :defer t)

(use-package lsp-clangd
  :defer t
  :config
  (add-to-list 'lsp-clients-clangd-args "--header-insertion=never"))

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

(use-package lsp-diagnostics
  :defer t
  :config
  (defun lsp-diagnostics--custom-flycheck-start (checker callback)
    "Customize `lsp-diagnostics--flycheck-start' to remove duplicated errors from `go-build' and `lsp'."
    (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)

    (let ((errors (->> flycheck-current-errors
                       (--filter (and      (eq (flycheck-error-buffer  it) (current-buffer))
                                           (not (eq (flycheck-error-checker it) checker))))
                       (-map (-juxt #'flycheck-error-line
                                    #'flycheck-error-column)))))
      (->> (lsp--get-buffer-diagnostics)
           (-map (-lambda ((&Diagnostic
                            :message
                            :severity?
                            :tags?
                            :code?
                            :range (&Range
                                    :start (&Position
                                            :line      start-line
                                            :character start-character)
                                    :end   (&Position
                                            :line      end-line
                                            :character end-character))))
                   (let ((line-beg (lsp-translate-line (1+ start-line)))
                         (line-end (lsp-translate-line (1+ end-line)))
                         (col-beg (1+ (lsp-translate-column start-character)))
                         (col-end (1+ (lsp-translate-column end-character))))
                     (unless (member (list line-beg col-beg) errors)
                       (flycheck-error-new
                        :buffer (current-buffer)
                        :checker checker
                        :filename buffer-file-name
                        :message message
                        :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                        :id code?
                        :line     line-beg
                        :end-line line-end
                        :column     col-beg
                        :end-column col-end)))))
           (-non-nil)
           (funcall callback 'finished))))

  (setq lsp-diagnostics-provider :flycheck)

  (add-hook 'lsp-diagnostics-mode-hook
            (lambda ()
              (when (or (eq lsp-diagnostics-provider :flycheck)
                        (and (eq lsp-diagnostics-provider :auto)
                             (featurep 'flycheck)))
                (cond
                 ((derived-mode-p 'go-mode)
                  (flycheck-select-checker 'go-vet)
                  (remove-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report t)
                  (remove-hook 'lsp-managed-mode-hook        #'lsp-diagnostics--flycheck-report t))
                 ((derived-mode-p 'rust-mode)
                  (flycheck-select-checker 'rust-clippy)
                  (flycheck-add-next-checker 'rust-clippy 'lsp))))))

  (advice-add #'lsp-diagnostics--flycheck-start :override #'lsp-diagnostics--custom-flycheck-start))

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

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)

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
         (python-mode     . lsp)
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

  (defun lps--focus-and-update-lsp-help-buffer (&rest _)
    "Force on '*lsp-help*' buffer"
    (-when-let (buf (get-buffer "*lsp-help*"))
      (pop-to-buffer buf)
      (let ((buffer-read-only nil)
            (beg (next-single-property-change (point-min) 'help-echo)))
        (while (and beg (< beg (point-max)))
          (let ((end (next-single-property-change (1+ beg) 'help-echo))
                (url (get-text-property beg 'help-echo)))
            (when (string-match-p "^https?://" url)
              (make-text-button beg end
                                'action
                                (lambda (&rest _)
                                  (browse-url--browser url))
                                'help-echo url
                                'follow-link t))
            (setq beg (next-single-property-change (1+ end) 'help-echo)))))))

  (defface lsp-face-workspace-modeline
    '((t (:extend t)))
    "TODO")

  (defun lsp--custom-workspace-print (workspace)
    "Visual representation WORKSPACE."
    (let* ((proc (lsp--workspace-cmd-proc workspace))
           (status (lsp--workspace-status workspace))
           (server-id (-> workspace
                          (lsp--workspace-client)
                          (lsp--client-server-id)
                          (symbol-name)))
           (pid (format "%s" (process-id proc))))
      (propertize
       (if (eq 'initialized status)
           (format "%s:%s" server-id pid)
         (format "%s:%s status:%s" server-id pid status))
       'face 'lsp-face-workspace-modeline)))

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
              (let ((val (match-string 1 md)))
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

  (defun lsp--custom-hover ()
    "Customize `lsp-hover' for Rust lang."
    (if (and lsp--hover-saved-bounds
             (lsp--point-in-bounds-p lsp--hover-saved-bounds))
        (lsp--eldoc-message lsp--eldoc-saved-message)
      (setq lsp--eldoc-saved-message nil
            lsp--hover-saved-bounds nil)
      (if (looking-at "[[:space:]\n]")
          (lsp--eldoc-message nil)
        (when (and lsp-eldoc-enable-hover (lsp--capability :hoverProvider))
          (let ((buf (current-buffer))
                (cur (point)))
            (lsp-request-async
             "textDocument/hover"
             (lsp--text-document-position-params)
             (-lambda ((hover &as &Hover? :range? :contents))
               (if (null hover)
                   (lsp--eldoc-message nil)
                 (when range?
                   (setq lsp--hover-saved-bounds (lsp--range-to-region range?))
                   (when (derived-mode-p 'rust-mode)
                     (with-current-buffer buf
                       (when (string-match-p "[{(<=>.?;)}]"
                                             (buffer-substring-no-properties (car lsp--hover-saved-bounds)
                                                                             (cdr lsp--hover-saved-bounds)))
                         (setq lsp--hover-saved-bounds (cons cur cur))))))
                 (lsp--eldoc-message (and contents
                                          (lsp--render-on-hover-content
                                           contents
                                           lsp-eldoc-render-all)))))
             :error-handler #'ignore
             :mode 'tick
             :cancel-token :eldoc-hover))))))

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
                          (floor (* (frame-height) max-mini-window-height)))
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
    (when-let ((max-chars (-> (cond
                               ((floatp max-mini-window-height)
                                (floor (* (frame-height) max-mini-window-height)))
                               ((numberp max-mini-window-height)
                                max-mini-window-height))
                              (1-)
                              (* (- (frame-width) 2)))))
      (when (< max-chars (length msg))
        (setq msg (concat (substring msg 0 max-chars) "\n" (propertize "(...)" 'face 'shadow)))))
    (setq lsp--eldoc-saved-message msg)
    (eldoc-message msg))

  (defalias 'lsp--custom-eldoc-message
    (if (version<= "28.0.50" emacs-version)
        #'lsp--custom-eldoc-message-emacs-28
      #'lsp--custom-eldoc-message-emacs-27))

  (defun lsp--signature->message-filter (msg)
    (if (stringp msg)
        (s-replace " │ " " | " msg)
      msg))

  (defvar lsp-signature-restart-enable nil)

  (defun lsp-signature-restart ()
    (when (and lsp-signature-restart-enable
               (save-excursion
                 (ignore-errors (backward-up-list))
                 (looking-at-p "(")))
      (ignore-errors
        (lsp-signature-activate))))

  (setq lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-file-watch-threshold nil
        lsp-idle-delay 0.2
        lsp-restart 'ignore
        lsp-rust-server 'rust-analyzer
        lsp-signature-doc-lines 10
        lsp-signature-function (lambda (msg)
                                 (when lsp--on-idle-timer
                                   (cancel-timer lsp--on-idle-timer)
                                   (setq lsp--on-idle-timer nil))
                                 (when eldoc-timer
                                   (cancel-timer eldoc-timer)
                                   (setq eldoc-timer nil))
                                 (when flycheck--idle-trigger-timer
                                   (cancel-timer flycheck--idle-trigger-timer)
                                   (setq flycheck--idle-trigger-timer nil))
                                 (eldoc-message msg)))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local show-error-list-fn #'lsp-treemacs-errors-list)
              (setq-local evil-lookup-func #'lsp-describe-thing-at-point)
              (cond
               ((derived-mode-p 'go-mode)
                (setq-local lsp-eldoc-render-all t)
                (add-hook 'before-save-hook #'lsp-format-buffer t t)
                (add-hook 'before-save-hook #'lsp-organize-imports t t)))
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          (when lsp-mode
                            (setq lsp-eldoc-enable-hover nil)
                            (when (and lsp-mode
                                       (lsp-feature? "textDocument/signatureHelp")
                                       (null lsp-signature-mode))
                              (ignore-errors
                                (setq lsp-signature-restart-enable t)
                                (lsp-signature-activate)))))
                        nil t)
              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          (setq lsp-eldoc-enable-hover t)
                          (when (and lsp-mode lsp-signature-mode)
                            (ignore-errors
                              (setq lsp-signature-restart-enable nil)
                              (lsp-signature-stop))))
                        nil t)
              (add-hook 'evil-operator-state-entry-hook
                        (lambda ()
                          (when (and lsp-mode
                                     lsp-ui-sideline-mode)
                            (lsp-ui-sideline--delete-ov)
                            (setq lsp-ui-sideline--tag nil)))
                        nil t)))

  (advice-add #'lsp :before-until (lambda () "Turn `lsp-mode' off" (bound-and-true-p git-timemachine-mode)))
  (advice-add #'lsp--eldoc-message :override 'lsp--custom-eldoc-message)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--adapter-render-on-hover-content)
  (advice-add #'lsp--signature->message :filter-return #'lsp--signature->message-filter)
  (advice-add #'lsp-describe-thing-at-point :after #'lps--focus-and-update-lsp-help-buffer)
  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-hover :override #'lsp--custom-hover)
  (advice-add #'lsp-signature-stop :after #'lsp-signature-restart)
  (advice-add #'lsp--workspace-print :override #'lsp--custom-workspace-print))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil))

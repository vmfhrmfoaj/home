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

(use-package lsp-clojure
  :defer t
  :init
  (setq lsp-clojure-custom-server-command '("bash" "-c" "clojure-lsp")))

(use-package lsp-diagnostics
  :defer t
  :init
  (defface lsp-punctuation-face
    '((t (:inherit shadow)))
    "TODO")

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

  (setq lsp-diagnostics-provider :flycheck
        lsp-diagnostics-attributes '((unnecessary :inherit 'lsp-punctuation-face)
                                     (deprecated  :strike-through t)))

  (add-hook 'lsp-diagnostics-mode-hook
            (lambda ()
              (when (or (eq lsp-diagnostics-provider :flycheck)
                        (and (eq lsp-diagnostics-provider :auto)
                             (featurep 'flycheck)))
                (cond
                 ((derived-mode-p 'go-mode)
                  (flycheck-select-checker 'go-golint)
                  (remove-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report t)
                  (remove-hook 'lsp-managed-mode-hook        #'lsp-diagnostics--flycheck-report t))
                 ((derived-mode-p 'python-mode)
                  (flycheck-select-checker 'python-flake8)
                  (flycheck-add-next-checker 'python-flake8 'lsp :append))))))

  (advice-add #'lsp-diagnostics--flycheck-start :override #'lsp-diagnostics--custom-flycheck-start)
  (advice-add #'lsp-modeline--diagnostics-update-modeline :override #'ignore))

(use-package lsp-headerline
  :defer t
  :commands (lsp-headerline--arrow-icon
             lsp-headerline--symbol-with-action)
  :init
  (defun lsp-headerline--custom-build-symbol-string ()
    (if (lsp-feature? "textDocument/documentSymbol")
        (-if-let* ((lsp--document-symbols-request-async t)
                   (symbols (lsp--get-document-symbols))
                   (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols)))
            (let* ((separator (concat " " (lsp-headerline--arrow-icon) " "))
                   (max-len (- spaceline-symbol-segment--max-symbol-length (length (concat "..." separator)))))
              (->> symbols-hierarchy
                   (reverse)
                   (-reduce-from
                    (-lambda ((&alist 'count count 'len len 'output output)
                              (symbol &as &DocumentSymbol :name :kind))
                      (if (<= max-len len)
                          `((count  . ,(1+ count))
                            (len    . ,len)
                            (output . ,output))
                        (let* ((icon (when-let ((disp (-some->> kind
                                                        (lsp-treemacs-symbol-icon)
                                                        (get-text-property 0 'display))))
                                       (if (stringp disp)
                                           (replace-regexp-in-string "\s\\|\t" "" disp)
                                         (propertize " " 'display
                                                     (cl-list* 'image
                                                               (plist-put
                                                                (cl-copy-list
                                                                 (cl-rest disp))
                                                                :background (bg-color-from 'powerline-active1)))))))
                               (symbol-name (concat icon "​​​" name)) ; zero width space * 3
                               (symbol-name (lsp-headerline--symbol-with-action symbol symbol-name))
                               (new-output (concat symbol-name (when output separator) output))
                               (new-len (length new-output)))
                          (if (<= max-len new-len)
                              `((count  . ,(1+ count))
                                (len    . ,(if (< 0 count)
                                               max-len
                                             new-len))
                                (output . ,(if (< 0 count)
                                               (concat "..." separator output)
                                             new-output)))
                            `((count  . ,(1+ count))
                              (len    . ,new-len)
                              (output . ,new-output))))))
                    '((count  . 0)
                      (len    . 0)
                      (output . nil)))
                   (alist-get 'output)))
          "")
      "")))

(use-package lsp-php
  :defer t
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda ()
                                       `(,(or (executable-find
                                               (cl-first lsp-intelephense-server-command))
                                              (lsp-package-path 'intelephense))
                                         ,@(cl-rest lsp-intelephense-server-command))))
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
                    :server-id 'iph
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'intelephense
                                                              callback error-callback)))))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)

(use-package lsp-java
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode             . lsp)
         (c++-mode           . lsp)
         ;; (clojure-mode       . lsp)
         ;; (clojurec-mode      . lsp)
         ;; (clojurescript-mode . lsp)
         (cperl-mode         . lsp)
         (go-mode            . lsp)
         (java-mode          . lsp)
         (js-mode            . lsp)
         (latex-mode         . lsp)
         (php-mode           . lsp)
         (python-mode        . lsp)
         (rust-mode          . lsp)
         (sh-mode            . lsp)
         (typescript-mode    . lsp))
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

  (defvar lsp--max-line-eldoc-msg
    (1- (cond
         ((floatp max-mini-window-height)
          (floor (* (frame-height) max-mini-window-height)))
         ((numberp max-mini-window-height)
          max-mini-window-height)
         (t 10))))

  (defun lsp--custom-eldoc-message (&optional msg)
    "Show MSG in eldoc."
    (setq lsp--eldoc-saved-message msg)
    (let ((lines (s-lines (or msg "")))
          (max-line lsp--max-line-eldoc-msg))
      (eldoc-message (when lines
                       (->> (if (<= (length lines) max-line)
                                lines
                              (-snoc (-take (max 1 (1- max-line)) lines) (propertize "(...)" 'face 'shadow)))
                            (-interpose "\n")
                            (apply #'concat))))))

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

  (defun lsp--workspace-print-without-style (workspace)
    "Visual representation WORKSPACE."
    (let* ((proc (lsp--workspace-cmd-proc workspace))
           (status (lsp--workspace-status workspace))
           (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name))
           (pid (format "%s" (process-id proc))))
      (if (eq 'initialized status)
          (format "%s:%s" server-id pid)
        (format "%s:%s status:%s" server-id pid status))))

  (setq lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-file-watch-threshold nil
        lsp-headerline-breadcrumb-enable nil
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

  (when-let ((mode (--first (eq (car it) 'lsp-mode) minor-mode-alist)))
    (setf (nth 1 mode) '(:eval (unless lsp--buffer-workspaces
                                 (propertize " LSP[Disconnected]" 'face 'error)))))

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
                          (when (and lsp-mode lsp-signature-mode)
                            (ignore-errors
                              (setq lsp-signature-restart-enable nil)
                              (lsp-signature-stop)))
                          (setq lsp-eldoc-enable-hover t))
                        nil t)
              (add-hook 'evil-operator-state-entry-hook
                        (lambda ()
                          (when (and lsp-mode
                                     lsp-ui-sideline-mode)
                            (lsp-ui-sideline--delete-ov)
                            (setq lsp-ui-sideline--tag nil)))
                        nil t)
              (let ((f (lambda ()
                         (let ((old spaceline-symbol-segment--symbol))
                           (setq spaceline-symbol-segment--symbol
                                 (lsp-headerline--custom-build-symbol-string))
                           (unless (string-equal old spaceline-symbol-segment--symbol)
                             (force-mode-line-update))))))
                (add-hook 'lsp-on-idle-hook     f nil t)
                (add-hook 'xref-after-jump-hook f nil t))))

  (advice-add #'lsp :before-until (lambda (&rest _) "Turn `lsp-mode' off" (bound-and-true-p git-timemachine-mode)))
  (advice-add #'lsp--eldoc-message :override 'lsp--custom-eldoc-message)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--adapter-render-on-hover-content)
  (advice-add #'lsp--signature->message :filter-return #'lsp--signature->message-filter)
  (advice-add #'lsp--workspace-print :override #'lsp--workspace-print-without-style)
  (advice-add #'lsp-describe-thing-at-point :after #'lps--focus-and-update-lsp-help-buffer)
  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-hover :override #'lsp--custom-hover)
  (advice-add #'lsp-signature-stop :after #'lsp-signature-restart))

(use-package lsp-pyls
  :defer t
  :config
  (add-to-list 'lsp-clients-python-library-directories "~/.local/lib"))

(use-package lsp-rust
  :defer t
  :config
  (setq lsp-rust-clippy-preference "on"))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil))

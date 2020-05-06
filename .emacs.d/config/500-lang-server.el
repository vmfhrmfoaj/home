(use-package company-lsp
  :ensure t
  :defer t)

(use-package dap-mode
  :ensure t
  :defer t)

(use-package flymake
  :defer t
  :config
  (defn flymake--wrap-goto-next-error (fn &rest args)
    "TODO"
    (cl-letf (((symbol-function #'message) (-const t)))
      (apply fn args))
    (when (fboundp #'eldoc-refresh)
      (eldoc-refresh)))

  (advice-add #'flymake-goto-next-error :around #'flymake--wrap-goto-next-error)

  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

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
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-intelephense-server-command))
                    :major-modes '(php-mode)
                    :priority -1
                    :notification-handlers (ht ("indexingStarted" #'ignore) ("indexingEnded" #'ignore))
                    :initialization-options (lambda ()
                                              (list :globalStoragePath lsp-intelephense-storage-path
                                                    :storagePath lsp-intelephense-storage-path
                                                    :clearCache lsp-intelephense-clear-cache))
                    :multi-root t
                    :server-id 'iph)))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode          . lsp)
         (c++-mode        . lsp)
         (cperl-mode      . lsp)
         (js-mode         . lsp)
         (js2-mode        . lsp)
         (php-mode        . lsp)
         (rust-mode       . lsp)
         (typescript-mode . lsp)
         (sh-mode         . lsp))
  :config
  (defvar lsp--custom-render--regex-1-for-php
    (concat "```php[ \t\r\n]*"                     ; ```php
            "\\(?:[ \t\r\n]*<\\?php[ \t\r\n]*\\)?" ; <?php
            "\\(\\(?:\n\\|.\\)+?\\)"               ; <CONTENTS>
            "\\(?:[ \t\r\n]*{\\s-*}[ \t\r\n]*\\)?" ; { }
            "[ \t\r\n]*\\(?:[ \t\r\n]*\\?>"        ; ?>
            "[ \t\r\n]*\\)?```")                   ; ```
    "TODO")
  (defvar lsp--custom-render--regex-2-for-php
    "\\(?:_@var_\\|_@param_\\)\\s-*`?\\s-*\\(.+?\\)\\s-*`?$"
    "TODO")

  (defvar lsp--custom-render--regex-for-rust
    (concat "```rust[ \t\r\n]*"      ; ```rust
            "\\(\\(?:\n\\|.\\)+?\\)" ; <CONTENTS>
            "[ \t\r\n]*?```")        ; ```
    "TODO")

  (defvar lsp--custom-render--regex-1-for-shell
    "^SYNOPSIS[ \t\r]*\n[ \t\r]*\\(\\(?:.\\|[\r\n]+\\)+?\\)[ \t\r]*\n[ \t\r]*\n"
    "TODO")
  (defvar lsp--custom-render--regex-2-for-shell
    "^[^ :]+?:[ \t]+\\([^\r\n]+\\)"
    "TODO")

  (defn lsp--adapter-render-on-hover-content (args)
    "TODO"
    (let ((contents (car args)))
      (cond
       ((and (derived-mode-p 'rust-mode)
             (hash-table-p contents))
        (puthash "language" "rust" contents)
        (when (string= "markdown" (or (gethash "kind" contents) ""))
          (let ((md (gethash "value" contents)))
            (when (string-match lsp--custom-render--regex-for-rust md)
              (let ((val (match-string 1 md)))
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

  (defn lsp--custom-eldoc-message (&optional msg)
    "Show MSG in eldoc."
    (setq lsp--eldoc-saved-message msg)
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
                            (apply #'concat))))))

  (defvar-local lsp--hover-saved-symbol nil
    "TODO")

  (defn lsp--reset-hover-cache ()
    "Clear `lsp--hover-saved-bounds' and `lsp--eldoc-saved-message'"
    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil
          lsp--hover-saved-symbol nil))

  (defvar lsp--hover-exclude-regex-for-rust
    (regexp-opt '("match" "let" "for" "if" "=") 'symbols)
    "TODO")

  (defn lsp--custom-hover-err-handler (_)
    "TODO"
    (lsp--reset-hover-cache)
    (lsp--eldoc-message nil))

  (defn lsp--custom-hover ()
    "Display hover info (based on `textDocument/hover')."
    (let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
           (symbol (when symbol-bounds
                     (buffer-substring-no-properties (car symbol-bounds) (cdr symbol-bounds)))))
      (if (and lsp--hover-saved-symbol (string= lsp--hover-saved-symbol symbol)
               lsp--hover-saved-bounds (lsp--point-in-bounds-p lsp--hover-saved-bounds))
          (lsp--eldoc-message lsp--eldoc-saved-message)
        (lsp--reset-hover-cache)
        (if (or (not symbol)
                (and (derived-mode-p 'rust-mode)
                     (string-match-p  lsp--hover-exclude-regex-for-rust symbol)))
            (lsp--eldoc-message nil)
          (when lsp-eldoc-enable-hover
            (setq lsp--hover-saved-symbol symbol)
            (cond
             ((member major-mode '(c-mode c++-mode))
              (when (lsp--capability "signatureHelpProvider")
                (lsp-request-async
                 "textDocument/signatureHelp"
                 (save-excursion
                   (goto-char (cdr symbol-bounds))
                   (when (looking-at "\\s-*(")
                     (down-list))
                   (lsp--text-document-position-params))
                 (-partial
                  (lambda (symbol symbol-bounds signature)
                    (let* ((sig (-some->> signature
                                  (gethash "signatures")
                                  (--map (gethash "label" it))
                                  (--filter (s-starts-with? symbol it))
                                  (--map (-some->> it
                                           (s-split "->")
                                           (-map #'s-trim)
                                           (reverse)
                                           (-interpose " ")
                                           (apply #'concat)
                                           (s-replace " * " "* ")))
                                  (-non-nil)
                                  (-interpose "\n")
                                  (apply #'concat)))
                           (sig (when sig
                                  (with-temp-buffer
                                    (c-mode)
                                    (insert sig)
                                    (font-lock-fontify-buffer)
                                    (setq sig (buffer-string))))))
                      (when sig
                        (setq lsp--hover-saved-bounds symbol-bounds))
                      (lsp--eldoc-message sig)))
                  symbol symbol-bounds)
                 :error-handler #'lsp--custom-hover-err-handler
                 :mode 'tick
                 :cancel-token :eldoc-hover)))
             (t
              (when (lsp--capability "hoverProvider")
                (lsp-request-async
                 "textDocument/hover"
                 (lsp--text-document-position-params)
                 (lambda (hover)
                   (if (null hover)
                       (lsp--eldoc-message nil)
                     (when-let (range (gethash "range" hover))
                       (setq lsp--hover-saved-bounds (lsp--range-to-region range)))
                     (let* ((contents (gethash "contents" hover)))
                       (condition-case nil
                           (lsp--eldoc-message (and contents
                                                    (lsp--render-on-hover-content contents
                                                                                  lsp-eldoc-render-all)))
                         (error
                          (lsp--reset-hover-cache)
                          (lsp--eldoc-message nil))))))
                 :error-handler #'lsp--custom-hover-err-handler
                 :mode 'tick
                 :cancel-token :eldoc-hover)))))))))

  (defn lsp--change-proj (cur-buf)
    "TODO"
    (let* ((persp-root (-some->> (persp-current-project)
                         (file-truename)))
           (proj-root (-some->> buffer-file-name
                        (file-name-directory)
                        (projectile-project-root)
                        (file-truename))))
      (when (and persp-root
                 proj-root
                 (not (s-starts-with? persp-root proj-root))
                 (not (s-starts-with? proj-root  persp-root)))
        (let ((buf-name (buffer-name))
              (old-persp (get-current-persp))
              (proj (abbreviate-file-name proj-root))
              (pos (point)))
          ;; NOTE
          ;;  To switch to old buffer to store persp before switching persp.
          ;;  `persp-add-buffer' will switch back to current buffer.
          (switch-to-buffer cur-buf)
          (save-excursion
            (persp-switch proj)
            (persp-add-buffer buf-name) ; see `switchorno' parameter
            (switch-to-buffer buf-name)
            (goto-char pos)
            (persp-remove-buffer buf-name old-persp))))))

  (defn lsp--wrap-find-xxx (f &rest args)
    "Fall back to `dumb-jump-go'."
    (let ((pos (point))
          (cur-buf (current-buffer)))
      (ignore-errors (not (apply f args)))
      (when (and (eq cur-buf (current-buffer))
                 (eq pos (point)))
        (ignore-errors (ring-remove xref--marker-ring 0))
        (message nil)
        (call-interactively #'dumb-jump-go))
      (lsp--change-proj cur-buf)))

  (defn lsp--custom-document-highlight ()
    "Disable `lsp-document-highlight'."
    (interactive))

  (defn lsp--custom-flymake-backend (report-fn &rest _args)
    "Custom `flymake' backend for ."
    (setq lsp--flymake-report-fn report-fn)
    (lsp--flymake-update-diagnostics))

  (defn lsp--clear-flymake-diags ()
     "Remove duplicated items in `diags' of `flymake--backend-state'."
     (-when-let (state (gethash 'lsp--flymake-backend flymake--backend-state))
       ;; NOTE
       ;;  `setf' is macro. I think the problem  too early expand the macro.
       ;; (eval '(setf (flymake--backend-state-diags state) (-distinct diags)))
       (aset state 4 nil)
       (flymake-delete-own-overlays)))

  (defn lsp--custom-handle-signature-update (signature)
    "Use `eldoc' instead `iv'."
    (let ((message (lsp--signature->message signature)))
      (if (s-present? message)
          (eldoc-message message)
        (lsp-signature-stop))))

  (defn lsp-custom-signature-stop ()
    "Use `eldoc' instead `iv'."
    (interactive)
    (lsp-cancel-request-by-token :signature)
    (with-current-buffer (or lsp--signature-last-buffer (current-buffer))
      (remove-hook 'lsp-on-idle-hook #'lsp-signature t))
    (remove-hook 'post-command-hook #'lsp--signature-maybe-stop)
    (eldoc-message nil)
    (lsp-signature-mode -1))

  (defn lsp--custom-signature->message (signature-help)
    "Customize to remove the document in the signature"
    (setq lsp--signature-last signature-help)
    (when (and signature-help (not (seq-empty-p (gethash "signatures" signature-help))))
      (-let* (((&hash "activeSignature" active-index
                      "activeParameter" active-parameter
                      "signatures")     signature-help)
              (active-index (or lsp--signature-last-index active-index 0))
              ((signature &as &hash? "label" "parameters") (seq-elt signatures active-index))
              (_ (when (and active-parameter (not (seq-empty-p parameters)))
                   (-when-let* ((param (when (and (< -1 active-parameter (length parameters)))
                                         (seq-elt parameters active-parameter)))
                                (selected-param-label (let ((label (gethash "label" param)))
                                                        (if (stringp label) label (append label nil))))
                                (start (if (stringp selected-param-label)
                                           (s-index-of selected-param-label label)
                                         (cl-first selected-param-label)))
                                (end (if (stringp selected-param-label)
                                         (+ start (length selected-param-label))
                                       (cl-second selected-param-label))))
                     (add-face-text-property start end 'eldoc-highlight-function-argument nil label))))
              (prefix (format "%s/%s%s"
                              (1+ active-index)
                              (length signatures)
                              (propertize " │ " 'face 'shadow)))
              (prefix2 (concat (s-repeat (- (length prefix) 2) " ")
                               (propertize " │ " 'face 'shadow)))
              (sig-lines (->> label (s-lines))))
        (setq lsp--signature-last-index active-index)
        (concat prefix
                (first sig-lines)
                (-some->> sig-lines
                  (-drop 1)
                  (apply #'concat "\n" prefix2))))))

  (defn lps--focus-lsp-help-buffer (&rest _)
    "Force on '*lsp-help*' buffer"
    (-when-let (buf (get-buffer "*lsp-help*"))
      (pop-to-buffer buf)))

  (setq lsp-diagnostic-package :flymake
        lsp-enable-on-type-formatting nil
        lsp-file-watch-threshold nil
        lsp-rust-server 'rust-analyzer)

  (add-to-list 'lsp-language-id-configuration '(cperl-mode . "perl"))
  (add-to-list 'lsp-language-id-configuration '(".*\\.pl$" . "perl"))

  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (when (and lsp-signature-auto-activate
                         (lsp-feature? "textDocument/signatureHelp")
                         (null lsp-signature-mode))
                (lsp-signature-activate))))
  (add-hook 'evil-insert-state-exit-hook #'lsp-signature-stop)

  (advice-add #'lsp--document-highlight :override #'lsp--custom-document-highlight)
  (advice-add #'lsp--eldoc-message   :override #'lsp--custom-eldoc-message)
  (advice-add #'lsp--flymake-backend :override #'lsp--custom-flymake-backend)
  (advice-add #'lsp--handle-signature-update :override #'lsp--custom-handle-signature-update)
  (advice-add #'lsp-signature-stop :override #'lsp-custom-signature-stop)
  (advice-add #'lsp--signature->message :override #'lsp--custom-signature->message)
  (advice-add #'lsp--flymake-update-diagnostics :before #'lsp--clear-flymake-diags)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--adapter-render-on-hover-content)
  (advice-add #'lsp-hover :override #'lsp--custom-hover)
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
  (defn lsp-ui-sideline--custom-diagnostics (fn bol eol)
    (if (and (or (eq lsp-diagnostic-package :auto)
                 (eq lsp-diagnostic-package :flycheck))
             (functionp 'flycheck-mode))
        (funcall fn bol eol)
      (let* ((line-num (line-number-at-pos bol t))
             (diagnostics (-some->> (lsp-diagnostics)
                                    (gethash buffer-file-name)
                                    (--filter (-when-let (range (lsp-diagnostic-range it))
                                                ;; NOTE: I think, the line number of `lsp-diagnostics' is zero-based numbering.
                                                (let ((beg (-some-> range (plist-get :start) (plist-get :line) (1+)))
                                                      (end (-some-> range (plist-get :end)   (plist-get :line) (1+))))
                                                  (when (<= beg line-num end)
                                                    it)))))))
        (dolist (diagnostic diagnostics)
          (let ((messages (-some->> diagnostic
                                    (lsp-diagnostic-message)
                                    (s-replace-regexp "[ \t]+" " ")
                                    (s-trim)
                                    (s-split "[\r\n]+")
                                    (-remove #'s-blank-str?)))
                (face (let ((level (lsp-diagnostic-severity diagnostic)))
                        (cond
                         ((eq 4 level) `(:inherit compilation-info :weight light))
                         ((eq 3 level) `(:inherit compilation-info :weight light))
                         ((eq 2 level) `(:inherit compilation-warning :weight light))
                         ((eq 1 level) `(:inherit compilation-error :weight light)))))
                (margin (lsp-ui-sideline--margin-width)))
            (dolist (message (->> (-drop 1 messages)
                                  (--map (concat it (propertize " ↩ " 'face `(:inherit shadow :family "Dejavu Sans Mono" :weight ,(face-attribute 'default :weight)))))
                                  (-cons* (-first-item messages))))
              (let* ((len (length message))
                     (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
                                     (progn
                                       (add-face-text-property 0 len 'lsp-ui-sideline-global nil message)
                                       (add-face-text-property 0 len face t message)
                                       message))))
                (-when-let (pos-ov (lsp-ui-sideline--find-line len bol eol))
                  (let ((ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
                    (overlay-put ov 'after-string string)
                    (overlay-put ov 'kind 'diagnotics)
                    (push ov lsp-ui-sideline--ovs))))))))))

  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil)

  (advice-add #'lsp-ui-sideline--diagnostics :around #'lsp-ui-sideline--custom-diagnostics))

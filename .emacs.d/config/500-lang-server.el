(use-package company-lsp
  :ensure t
  :after (helm-company)
  :config
  (push 'company-lsp company-backends))

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

(use-package lsp-java
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (require 'lsp-java)
              (lsp))))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode          . lsp)
         (c++-mode        . lsp)
         (js-mode         . lsp)
         (js2-mode        . lsp)
         (php-mode        . lsp)
         (rust-mode       . lsp)
         (typescript-mode . lsp)
         (sh-mode         . lsp))
  :commands lsp
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
  (defvar lsp--filter--regex-for-rust
    "^\\(()\\|{unknown}\\)$")

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
                (when (string-match lsp--filter--regex-for-rust val)
                  (error (concat "'" val "' is invalid.")))
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

  (defn lsp--reset-hover-cache ()
    "Clear `lsp--hover-saved-bounds' and `lsp--eldoc-saved-message'"
    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil))

  (defn lsp--custom-hover ()
    "Display hover info (based on `textDocument/hover')."
    (if (and lsp--hover-saved-bounds
             (lsp--point-in-bounds-p lsp--hover-saved-bounds))
        (lsp--eldoc-message lsp--eldoc-saved-message)
      (lsp--reset-hover-cache)
      (if (not (thing-at-point 'symbol))
          (lsp--eldoc-message nil)
        (when (and lsp-eldoc-enable-hover (lsp--capability "hoverProvider"))
          (lsp-request-async
           "textDocument/hover"
           (lsp--text-document-position-params)
           (lambda (hover)
             (if (null hover)
                 (lsp--eldoc-message nil)
               (when-let (range (gethash "range" hover))
                 (setq lsp--hover-saved-bounds (lsp--range-to-region range)))
               (-let* ((contents (gethash "contents" hover)))
                 (condition-case nil
                     (lsp--eldoc-message (and contents
                                              (lsp--render-on-hover-content contents
                                                                            lsp-eldoc-render-all)))
                   (error
                    (lsp--reset-hover-cache)
                    (lsp--eldoc-message nil))))))
           :error-handler #'ignore
           :mode 'tick
           :cancel-token :eldoc-hover)))))

  (defn lsp--wrap-find-xxx-for-fallback (f &rest args)
    "Fall back to `dumb-jump-go'."
    (let ((pos (point))
          (cur-buf (current-buffer))
          (success? (ignore-errors (not (apply f args)))))
      (when (and (not success?)
                 (eq cur-buf (current-buffer))
                 (= pos (point)))
        (message nil)
        (call-interactively #'dumb-jump-go))))

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

  (setq lsp-file-watch-threshold nil
        lsp-rust-server 'rust-analyzer)

  (advice-add #'lsp--document-highlight :override #'lsp--custom-document-highlight)
  (advice-add #'lsp--eldoc-message   :override #'lsp--custom-eldoc-message)
  (advice-add #'lsp--flymake-backend :override #'lsp--custom-flymake-backend)
  (advice-add #'lsp--flymake-update-diagnostics :before #'lsp--clear-flymake-diags)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--adapter-render-on-hover-content)
  (advice-add #'lsp-hover :override #'lsp--custom-hover)
  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx-for-fallback))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :config
  (defn lsp-ui-sideline--custom-diagnostics (fn bol eol)
    (if (not lsp-prefer-flymake)
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

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'dap-mode nil t)
  (require 'lsp-clangd nil t)
  (require 'lsp-completion nil t)
  (require 'lsp-clojure nil t)
  (require 'lsp-diagnostics nil t)
  (require 'lsp-php nil t)
  (require 'lsp-ivy nil t)
  (require 'lsp-java nil t)
  (require 'lsp-python-ms nil t)
  (require 'lsp-mode nil t)
  (require 'lsp-pyls nil t)
  (require 'lsp-rust nil t)
  (require 'lsp-ui nil t))

(use-package dap-mode
  :ensure t
  :defer t)

(use-package lsp-clangd
  :defer t
  :config
  (add-to-list 'lsp-clients-clangd-args "--header-insertion=never"))

(use-package lsp-completion
  :defer t
  :config
  (advice-add #'lsp-completion--annotate :filter-return
              (lambda (rv)
                "Add a separator at front."
                (concat " ·" rv)))

  (advice-add #'lsp-completion--sort-completions :around
              (lambda (fn completions)
                "To avoid errors."
                (when (seqp completions)
                  (funcall fn completions)))))

(use-package lsp-clojure
  :defer t
  :init
  (setq lsp-clojure-custom-server-command '("clojure-lsp")))

(use-package lsp-diagnostics
  :defer t
  :init
  (defface lsp-punctuation-face
    '((t (:inherit shadow)))
    "TODO")

  :config
  (defun lsp-diagnostics--custom-flycheck-start (checker callback)
    "Customize `lsp-diagnostics--flycheck-start' to remove duplicated errors from lint tools and `lsp'."
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

  (advice-add #'lsp-diagnostics--flycheck-start :override #'lsp-diagnostics--custom-flycheck-start)
  (advice-add #'lsp-modeline--diagnostics-update-modeline :override #'ignore)
  (advice-add #'lsp-diagnostics--enable :before-until
              (lambda ()
                "Do not turn `lsp-diagnostics-mode' on in specific modes."
                (or (derived-mode-p 'clojure-mode)
                    (derived-mode-p 'cperl-mode)
                    (derived-mode-p 'perl-mode)))))

(use-package lsp-php
  :defer t
  :init
  (setq lsp-intelephense-format-enable nil)

  :config
  (let* ((old-init-opts (funcall (lsp--client-initialization-options (gethash 'iph lsp-clients))))
         (new-init-opts (append old-init-opts `(:globalStoragePath ,lsp-intelephense-storage-path))))
    (setf (lsp--client-initialization-options (gethash 'iph lsp-clients)) (-const new-init-opts))))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :config
  (lsp-defun lsp-ivy--format-symbol-match-for-doc-syms
    ((sym &as &DocumentSymbol :kind :range (&Range :start (&Position :line :character))))
    (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-to-face)) kind 0))
           (type (elt lsp-ivy-symbol-kind-to-face sanitized-kind))
           (typestr (if lsp-ivy-show-symbol-kind
                        (propertize " " 'face (cdr type) 'display (format "[%s] " (car type)))
                      ""))
           (posstr (if lsp-ivy-show-symbol-filename
                       (propertize " " 'face 'lsp-details-face 'display (format " · Line: %s" line)))))
      (concat typestr (lsp-render-symbol sym ".") posstr)))

  (lsp-defun lsp-ivy--custom-format-symbol-match
    ((sym &as &SymbolInformation :kind :location (&Location :uri))
     project-root)
    (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-to-face)) kind 0))
           (type (elt lsp-ivy-symbol-kind-to-face sanitized-kind))
           (typestr (if lsp-ivy-show-symbol-kind
                        (propertize " " 'face (cdr type) 'display (format "[%s] " (car type)))
                      ""))
           (pathstr (if lsp-ivy-show-symbol-filename
                        (propertize " "
                                    'face font-lock-comment-face
                                    'display (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root)))
                      "")))
      (concat typestr (lsp-render-symbol-information sym ".") pathstr)))

  (defun lsp-ivy-workspace-symbol-for-cur-file (&optional initial-input)
    (interactive)
    (when-let ((buf-file (buffer-file-name)))
      (let* ((workspaces (lsp-workspaces))
             (prev-query nil)
             (unfiltered-candidates '())
             (filtered-candidates nil)
             (workspace-root (lsp-workspace-root))
             (update-candidates
              (lambda (all-candidates filter-regexps?)
                (let ((lsp-ivy-show-symbol-filename nil))
                  (setq filtered-candidates
                        (-some->> all-candidates
                                  (--filter (-let (((&SymbolInformation :location (&Location :uri)) it))
                                              (string= buf-file (lsp--uri-to-path uri))))
                                  (--keep (lsp-ivy--transform-candidate it filter-regexps? workspace-root)))))
                (ivy-update-candidates filtered-candidates))))
        (ivy-read
         "File symbol: "
         (lambda (user-input)
           (let* ((parts (split-string user-input))
                  (query (or (car parts) ""))
                  (filter-regexps? (mapcar #'regexp-quote (cdr parts))))
             (if (string-equal prev-query query)
                 (funcall update-candidates unfiltered-candidates filter-regexps?)
               (with-lsp-workspaces workspaces
                 (lsp-request-async
                  "workspace/symbol"
                  (lsp-make-workspace-symbol-params :query query)
                  (lambda (result)
                    (setq unfiltered-candidates result)
                    (funcall update-candidates unfiltered-candidates filter-regexps?))
                  :mode 'detached
                  :cancel-token :workspace-symbol)))
             (setq prev-query query))
           (or filtered-candidates 0))
         :dynamic-collection t
         :require-match t
         :initial-input (or initial-input
                            (when (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))))
         :action #'lsp-ivy--workspace-symbol-action
         :caller 'lsp-ivy-workspace-symbol))))

  (defun lsp-ivy-doc-symbol (&optional initial-input)
    (interactive)
    (when (buffer-file-name)
      (ivy-read
       "File symbol: "
       (let ((root (lsp-workspace-root)))
         (--map (let ((txt (if (gethash "location" it)
                               (lsp-ivy--format-symbol-match it root)
                             (lsp-ivy--format-symbol-match-for-doc-syms it))))
                  (propertize txt 'lsp-doc-sym it))
                (lsp--get-document-symbols)))
       :action (lambda (it)
                 (let ((sym-info-or-doc-sym (get-text-property 0 'lsp-doc-sym it)))
                   (if (gethash "location" sym-info-or-doc-sym)
                       (lsp-ivy--workspace-symbol-action sym-info-or-doc-sym)
                     (-let (((&DocumentSymbol :range (&Range :start (&Position :line :character)))
                             sym-info-or-doc-sym))
                       (goto-line (1+ line))
                       (move-to-column character)))))
       :initial-input (or initial-input
                          (when (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))))
       :caller 'lsp-ivy-file-symbol)))

  (advice-add #'lsp-ivy--format-symbol-match :override #'lsp-ivy--custom-format-symbol-match))

(use-package lsp-java
  :ensure t
  :defer t)

(use-package lsp-python-ms
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . (lambda ()
                     (when (eq 'c-mode major-mode) ; to prevent to enable `lsp' on derived mode of `c-mode'
                       (lsp))))
         (c++-mode           . lsp)
         (clojure-mode       . lsp)
         (clojurec-mode      . lsp)
         (clojurescript-mode . lsp)
         (cperl-mode         . lsp)
         (dockerfile-mode    . lsp)
         (go-mode            . lsp)
         (java-mode . (lambda ()
                        (when (require 'lsp-java nil t)
                          (lsp))))
         (js-mode . lsp)
         ;; FIXME
         ;;  How to run it on docker?
         ;; (latex-mode . lsp)
         (php-mode   . lsp)
         (python-mode . (lambda ()
                          (when (require 'lsp-python-ms nil t)
                            (lsp))))
         (rust-mode . lsp)
         (sh-mode . (lambda ()
                      (when (and (eq 'sh-mode major-mode) ; to prevent to enable `lsp' on derived mode of `sh-mode'
                                 (memq sh-shell '(bash)))
                        (lsp))))
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
                 (= pos (point)))
        (ignore-errors (ring-remove xref--marker-ring 0))
        (message nil)
        (call-interactively #'dumb-jump-go))))

  (defun lps--focus-and-update-lsp-help-buffer (&rest _)
    "Force on '*lsp-help*' buffer"
    (-when-let (buf (get-buffer "*lsp-help*"))
      (pop-to-buffer-same-window buf)
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

  (defvar lsp--max-line-eldoc-msg
    (1- (cond
         ((floatp eldoc-echo-area-use-multiline-p)
          (floor (* (frame-height) eldoc-echo-area-use-multiline-p)))
         ((numberp eldoc-echo-area-use-multiline-p)
          eldoc-echo-area-use-multiline-p)
         ((floatp max-mini-window-height)
          (floor (* (frame-height) max-mini-window-height)))
         ((numberp max-mini-window-height)
          max-mini-window-height)
         (t 10))))

  (defun lsp--custom-eldoc-message-for-emacs-27 (&optional msg)
    "Show MSG in eldoc."
    (unless isearch-mode
      (cond
       ((derived-mode-p 'c-mode 'c++-mode)
        (when msg
          (setq msg (s-replace-regexp "\\s-*\\\\\\s-*" " " msg)))))
      (setq lsp--eldoc-saved-message msg)
      (let ((lines (s-lines (or msg "")))
            (max-line lsp--max-line-eldoc-msg))
        (eldoc-message (when lines
                         (->> (if (<= (length lines) max-line)
                                  lines
                                (-snoc (-take (max 1 (1- max-line)) lines) (propertize "(...)" 'face 'shadow)))
                              (-interpose "\n")
                              (apply #'concat)))))))

  (defun lsp--custom-eldoc-message-for-emacs-28 (&optional msg)
    "Show MSG in eldoc."
    (unless isearch-mode
      (cond
       ((derived-mode-p 'c-mode 'c++-mode)
        (when msg
          (setq msg (s-replace-regexp "\\s-*\\\\\\s-*" " " msg)))))
      (when-let ((max-chars (* lsp--max-line-eldoc-msg (frame-width))))
        (when (< max-chars (length msg))
          (setq msg (concat (substring msg 0 max-chars) "\n" (propertize "(...)" 'face 'shadow)))))
      (setq lsp--eldoc-saved-message msg)
      (eldoc-message msg)))

  (defalias 'lsp--custom-eldoc-message
    (if (version<= "28.0.50" emacs-version)
        #'lsp--custom-eldoc-message-for-emacs-28
      #'lsp--custom-eldoc-message-for-emacs-27))

  (defun lsp--signature->message-filter (msg)
    (if (stringp msg)
        (s-replace " │ " " | " msg)
      msg))

  (defvar lsp-signature-prevent-stop-enable nil)

  (let ((begin-parent (string-to-char "(")))
    (defun lsp-signature-prevent-stop ()
      (and lsp-signature-prevent-stop-enable
           (-some->> (syntax-ppss)
                     (nth 1)
                     (char-after)
                     (char-equal begin-parent)))))

  (defun lsp--workspace-print-without-style (workspace)
    "Visual representation WORKSPACE."
    (let* ((proc (lsp--workspace-cmd-proc workspace))
           (status (lsp--workspace-status workspace))
           (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name))
           (pid (format "%s" (process-id proc))))
      (if (eq 'initialized status)
          (format "%s:%s" server-id pid)
        (format "%s:%s status:%s" server-id pid status))))

  (defun lsp--custom-build-symbol-string ()
    (if (and (derived-mode-p 'clojure-mode)
             (string= "project.clj" (file-name-nondirectory (buffer-file-name))))
        ;; NOTE
        ;;  workaround for `clojure-lsp'
        (->> (clojure-project-root-path)
             (directory-file-name)
             (file-name-nondirectory))
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols)))
              (let* ((separator " › ")
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
                          (let* ((new-output (concat name (when output separator) output))
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

  (setq lsp-completion-provider :none
        lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-file-watch-threshold nil
        lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.35
        lsp-modeline-code-actions-enable nil
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
                                 (-some->> msg
                                           (s-lines)
                                           (-drop-while #'s-blank-str?)
                                           (-first-item)
                                           (eldoc-message))))

  (when-let ((mode (--first (eq (car it) 'lsp-mode) minor-mode-alist)))
    (setf (nth 1 mode) '(:eval (unless lsp--buffer-workspaces
                                 (propertize " LSP[Disconnected]" 'face 'error)))))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  (add-hook 'cider-mode-hook
            (lambda ()
              (dolist (buf (cider-util--clojure-buffers))
                (when (and (bound-and-true-p lsp-mode)
                           (cider-connected-p))
                  (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)
                  (setq-local completion-at-point-functions '(cider-complete-at-point)
                              evil-lookup-func #'cider-doc-at-point)))))
  (add-hook 'cider-disconnected-hook
            (lambda ()
              (dolist (buf (cider-util--clojure-buffers))
                (with-current-buffer buf
                  (add-function :before-until (local 'eldoc-documentation-function) #'lsp-eldoc-function)
                  (setq-local completion-at-point-functions '(lsp-completion-at-point)
                              evil-lookup-func #'lsp-describe-thing-at-point)))))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local show-error-list-fn #'lsp-treemacs-errors-list)
              (setq-local evil-lookup-func #'lsp-describe-thing-at-point)

              (cond
               ((derived-mode-p 'go-mode)
                (setq-local lsp-eldoc-render-all t)
                (add-hook 'before-save-hook #'lsp-format-buffer t t)
                (add-hook 'before-save-hook #'lsp-organize-imports t t))
               ((and (bound-and-true-p cider-mode)
                     (cider-connected-p))
                (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)
                (setq-local completion-at-point-functions '(cider-complete-at-point)
                            evil-lookup-func #'cider-doc-at-point)))

              (add-hook 'evil-insert-state-entry-hook
                        ;; NOTE
                        ;;  ?( cause a problem on default select the region function
                        (let ((begin-parent (string-to-char "(")))
                          (lambda ()
                            (when lsp-mode
                              (setq lsp-eldoc-enable-hover nil)
                              (when (and (lsp-feature? "textDocument/signatureHelp")
                                         (not (and (bound-and-true-p cider-mode)
                                                   (cider-connected-p)))
                                         (and (boundp 'lsp-signature-mode)
                                              (null lsp-signature-mode))
                                         (-some->> (syntax-ppss)
                                                   (nth 1)
                                                   (char-after)
                                                   (char-equal begin-parent)))
                                (setq lsp-signature-prevent-stop-enable t)
                                (ignore-errors (lsp-signature-activate)))
                              (when (bound-and-true-p lsp-diagnostics-mode)
                                (remove-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report t))
                              (when (bound-and-true-p lsp-enable-symbol-highlighting)
                                (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
                                (lsp--remove-overlays 'lsp-highlight)))))
                        nil t)

              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          (when lsp-mode
                            (when (bound-and-true-p lsp-diagnostics-mode)
                              (add-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report nil t)
                              (let ((lsp-idle-delay 0))
                                (lsp-diagnostics--flycheck-report)))
                            (when (bound-and-true-p lsp-signature-mode)
                              (setq lsp-signature-prevent-stop-enable nil)
                              (ignore-errors (lsp-signature-stop)))
                            (when (bound-and-true-p lsp-enable-symbol-highlighting)
                              (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t))
                            (setq lsp-eldoc-enable-hover t)))
                        nil t)

              (let ((f (lambda ()
                         (let ((old spaceline-symbol-segment--symbol))
                           (setq spaceline-symbol-segment--symbol
                                 (lsp--custom-build-symbol-string))
                           (unless (string-equal old spaceline-symbol-segment--symbol)
                             (force-mode-line-update))))))
                (add-hook 'lsp-on-idle-hook     f nil t)
                (add-hook 'xref-after-jump-hook f nil t))))

  (advice-add #'lsp :before-until (lambda (&rest _) "Turn `lsp-mode' off" (bound-and-true-p git-timemachine-mode)))
  (advice-add #'lsp--eldoc-message :override 'lsp--custom-eldoc-message)
  (advice-add #'lsp--signature->message :filter-return #'lsp--signature->message-filter)
  (advice-add #'lsp--workspace-print :override #'lsp--workspace-print-without-style)
  (advice-add #'lsp-describe-thing-at-point :after #'lps--focus-and-update-lsp-help-buffer)
  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx)
  (advice-add #'lsp-signature-stop :before-until #'lsp-signature-prevent-stop)
  (advice-add #'lsp--info :override #'ignore)
  (advice-add #'lsp--handle-signature-update :around
              (lambda (f &rest args)
                "Turn `lsp-signature' off when rasing an erorr"
                (condition-case nil
                    (apply f args)
                  (error
                   (setq lsp-signature-prevent-stop-enable nil)
                   (lsp-signature-stop)))))
  ;; NOTE
  ;;  Sometimes, `lsp-workspaces' return list of nil. (e.g., '(nil))
  (advice-add #'lsp-workspaces :filter-return #'-non-nil))

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

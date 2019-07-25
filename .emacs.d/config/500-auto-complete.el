(use-package company
  :ensure t
  :config
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (setq company-idle-delay nil
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-etags-ignore-case t)
  (global-company-mode 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-company
  :ensure t
  :after (company helm)
  :init
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
              :candidate-number-limit helm-company-candidate-number-limit))))

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

  :config
  (add-hook 'helm-cleanup-hook
            (lambda ()
              (with-helm-current-buffer
                (unless (minibufferp)
                  (company-abort)))))

  ;; NOTE
  ;;  Turn company popup off completely.
  (remove-hook 'pre-command-hook 'company-pre-command)
  (remove-hook 'post-command-hook 'company-post-command)
  (advice-add #'company-mode-on :after
              (byte-compile
               (lambda ()
                 (remove-hook 'pre-command-hook 'company-pre-command t)
                 (remove-hook 'post-command-hook 'company-post-command t)))))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-mode
  :ensure t
  :hook ((js-mode         . lsp)
         (js2-mode        . lsp)
         (php-mode        . lsp)
         (rust-mode       . lsp)
         (typescript-mode . lsp)
         (sh-mode         . lsp))
  :commands lsp
  :init
  (defn lsp--custom-render-on-hover-content (args)
    (let ((contents (car args)))
      (if (not (seqp contents))
          args
        (apply #'list (-interpose "\n" (append contents nil)) (-drop 1 args)))))

  :config
  (setq lsp-enable-snippet nil)
  (advice-add #'lsp--render-on-hover-content :filter-args
              #'lsp--custom-render-on-hover-content))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (defn lsp-ui-sideline--custom-diagnostics (fn bol eol)
    (if (not lsp-prefer-flymake)
        (funcall fn bol eol)
      (let* ((offset 0)
             (line-num (line-number-at-pos bol t))
             (diagnostics (-some->> (lsp-diagnostics)
                                    (gethash buffer-file-name)
                                    (--filter (-when-let (range (lsp-diagnostic-range it))
                                                ;; NOTE: I think, the line number of `lsp-diagnostics' is zero-based numbering.
                                                (let ((beg (-some-> range (plist-get :start) (plist-get :line) (1+)))
                                                      (end (-some-> range (plist-get :end)   (plist-get :line) (1+))))
                                                  (when (<= beg line-num end)
                                                    it)))))))
        (dolist (diagnostic diagnostics)
          (let* ((message (-some->> diagnostic
                                    (lsp-diagnostic-message)
                                    (s-replace-regexp "[ \t]+" " ")
                                    (s-trim)))
                 (len (length message))
                 (level (lsp-diagnostic-severity diagnostic))
                 (face (cond
                        ((eq 4 level) 'success)
                        ((eq 3 level) 'success)
                        ((eq 2 level) 'font-lock-warning-face)
                        ((eq 1 level) 'error)))
                 (margin (lsp-ui-sideline--margin-width))
                 (message (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil message)
                                 (add-face-text-property 0 len face nil message)
                                 message))
                 (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
                                 message))
                 (pos-ov (lsp-ui-sideline--find-line len bol eol nil offset))
                 (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
            (when pos-ov
              (setq offset (car (cdr pos-ov)))
              (overlay-put ov 'after-string string)
              (overlay-put ov 'kind 'diagnotics)
              (push ov lsp-ui-sideline--ovs)))))))

  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil)
  (advice-add #'lsp-ui-sideline--diagnostics :around
              #'lsp-ui-sideline--custom-diagnostics))

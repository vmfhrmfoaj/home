(use-package flymake
  :defer t
  :config
  (defn flymake--wrap-goto-next-error (fn &rest args)
    "TODO"
    (cl-letf (((symbol-function #'message) (-const t)))
      (apply fn args))
    (when (fboundp #'eldoc-refresh)
      (eldoc-refresh)))

  (advice-add #'flymake-goto-next-error :around #'flymake--wrap-goto-next-error))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

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

  (defn lsp--custom-render-on-hover-content (args)
    "TODO"
    (let ((contents (car args)))
      (when (and (derived-mode-p 'php-mode)
                 (hash-table-p contents))
        (puthash "language" "php" contents)
        (when (string= "markdown" (or (gethash "kind" contents) ""))
          (let ((md (gethash "value" contents)))
            (when (or (string-match lsp--custom-render--regex-1-for-php md)
                      (string-match lsp--custom-render--regex-2-for-php md))
             (remhash "kind" contents)
             (puthash "value" (match-string 1 md) contents)))))
      (if (not (listp contents))
          (apply #'list contents (cdr args))
        (apply #'list (-interpose "\n" (append contents nil)) (cdr args)))))

  (defn lsp--custom-render-string (args)
    "TODO"
    (let ((str (car args))
          (lang (cadr args)))
      (cond
       ((and (derived-mode-p 'sh-mode)
             (stringp str)
             (or (string-match "^SYNOPSIS[ \t\r]*\n[ \t\r]*\\(\\(?:.\\|[\r\n]+\\)+?\\)[ \t\r]*\n[ \t\r]*\n" str)
                 (let ((res (string-match "^[^ :]+?:[ \t]+\\([^\r\n]+\\)$" str)))
                   (and (numberp res) (= 0 res)))))
        (list (->> str
                   (match-string 1)
                   (s-lines)
                   (-map #'s-trim)
                   (-interpose "\n")
                   (apply #'concat))
              lang))
       (t args))))

  (defn lsp--custom-eldoc-message (&optional msg)
    "Show MSG in eldoc."
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

  (defn lsp--custom-flymake-backend (report-fn &rest _args)
    "Custom `flymake' backend for ."
    (setq lsp--flymake-report-fn report-fn)
    (lsp--flymake-update-diagnostics))

  (setq lsp-eldoc-prefer-signature-help nil
        lsp-enable-snippet nil
        lsp-file-watch-threshold nil)

  (advice-add #'lsp--eldoc-message   :override #'lsp--custom-eldoc-message)
  (advice-add #'lsp--flymake-backend :override #'lsp--custom-flymake-backend)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--custom-render-on-hover-content)
  (advice-add #'lsp--render-string           :filter-args #'lsp--custom-render-string)
  (advice-add #'lsp-find-definition      :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-declaration     :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-implementation  :around #'lsp--wrap-find-xxx-for-fallback)
  (advice-add #'lsp-find-type-definition :around #'lsp--wrap-find-xxx-for-fallback))

(use-package lsp-ui
  :ensure t
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
                         ((eq 4 level) `(:inherit warning :weight ,(face-attribute 'default :weight)))
                         ((eq 3 level) `(:inherit warning :weight bold))
                         ((eq 2 level) `(:inherit error   :weight ,(face-attribute 'default :weight)))
                         ((eq 1 level) `(:inherit error   :weight bold)))))
                (margin (lsp-ui-sideline--margin-width)))
            (dolist (message (->> (-drop 1 messages)
                                  (--map (concat it (propertize " ↩ " 'face `(:inherit shadow :weight ,(face-attribute 'default :weight)))))
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

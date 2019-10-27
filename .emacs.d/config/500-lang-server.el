(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

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

  :config
  (setq lsp-enable-snippet nil
        lsp-file-watch-threshold nil)
  (advice-add #'lsp--eldoc-message :override #'lsp--custom-eldoc-message)
  (advice-add #'lsp--render-on-hover-content :filter-args #'lsp--custom-render-on-hover-content)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (let ((f (byte-compile
                        (lambda (&rest _)
                          ;; NOTE:
                          ;;  May be it cause the performance issue!
                          (flymake-start t t)))))
                (add-hook 'after-save-hook f nil :local)
                (add-hook 'after-change-functions f nil :local))))
  (let ((f (byte-compile
            (lambda (f &rest args)
              "fallback"
              (let ((success? (ignore-errors (not (apply f args)))))
                (unless success?
                  (message nil)
                  (call-interactively #'dumb-jump-go)))))))
    (advice-add #'lsp-find-definition      :around f)
    (advice-add #'lsp-find-declaration     :around f)
    (advice-add #'lsp-find-implementation  :around f)
    (advice-add #'lsp-find-type-definition :around f)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
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
                 (message (progn
                            (add-face-text-property 0 len 'lsp-ui-sideline-global nil message)
                            (add-face-text-property 0 len face nil message)
                            message))
                 (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
                                 message))
                 (pos-ov (lsp-ui-sideline--find-line len bol eol))
                 (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
            (when pos-ov
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

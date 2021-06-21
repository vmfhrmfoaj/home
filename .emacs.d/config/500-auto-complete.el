;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'company nil t)
  (require 'counsel nil t))

(use-package company
  :ensure t
  :hook ((prog-mode   . company-mode-on)
         (eshell-mode . company-mode-on))
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  ;; copy from spaceamcs
  (defun company-backend-with-yas (backends)
    (if (and (listp backends) (memq 'company-yasnippet backends))
	    backends
	  (append (if (consp backends)
		          backends
		        (list backends))
		      '(:with company-yasnippet))))

  (setq company-dabbrev-code-ignore-case t
        company-dabbrev-code-other-buffers nil
        company-echo-delay nil
        company-etags-ignore-case t
        company-format-margin-function nil
        company-idle-delay nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-flip-when-above t
        company-backends (mapcar #'company-backend-with-yas company-backends))

  (add-hook 'company-after-completion-hook
            (lambda (_ignored)
              (when (timerp eldoc-timer)
                (cancel-timer eldoc-timer)
                (setq eldoc-timer nil))
              (run-at-time 0.1 nil (-partial #'call-interactively #'eldoc-refresh))))

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (company--active-p)
                (company-cancel))))

  (add-hook 'evil-local-mode-hook
            (lambda ()
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist))))

  (advice-add #'company--insert-candidate :filter-args
              (lambda (args)
                "Remove the whitespace"
                (if-let ((candidate (car args)))
                    (list (string-trim candidate))
                  (list ""))))

  (with-eval-after-load "lsp-mode"
    (advice-add #'company-echo-show :around
                (lambda (fn &rest args)
                  "To disable `company-echo' when `lsp-signature-mode' is being enabled."
                  (unless (bound-and-true-p lsp-signature-mode)
                    (apply fn args))))))

(use-package counsel
  :after company
  :init
  (defface counsel-company-annotation-face
    '((t :inherit shadow))
    "TODO")

  :config
  (defun counsel--company-display-transformer (s)
    (concat s (let ((annot (company-call-backend 'annotation s)))
                (when annot
                  (propertize " " 'face 'counsel-company-annotation-face 'display (company--clean-string annot))))))

  (defun counsel--custom-company ()
    "Custom `counsel-company'"
    (interactive)
    (company-mode 1)
    (unless company-candidates
      (company-complete))
    (if company-candidates
        (let ((prefix company-prefix)
              (len (cond ((let (l)
                            (and company-common
                                 (string= company-common
                                          (buffer-substring
                                           (- (point) (setq l (length company-common)))
                                           (point)))
                                 l)))
                         (company-prefix
                          (length company-prefix)))))
          (when len
            (setq ivy-completion-beg (- (point) len))
            (setq ivy-completion-end (point))
            (ivy-read "Candidate: " company-candidates
                      :action (lambda (str)
                                (ivy-completion-in-region-action str)
                                (setq company-prefix prefix
                                      company-backend (get-text-property 0 'company-backend str))
                                (company-cancel str))
                      :caller 'counsel-company
                      :initial-input (-> (or company-common company-prefix)
                                         (downcase)
                                         (propertize 'read-only t 'rear-nonsticky '(read-only))
                                         (concat " "))
                      :sort t)))
      (message "There is no candidate.")))

  (-update->> ivy--display-transformers-alist
              (--remove (-let (((caller . _rest) it))
                          (eq 'counsel-company caller))))

  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-company . ivy--shorter-matches-first))
  (add-to-list 'ivy--display-transformers-alist '(counsel-company . counsel--company-display-transformer))

  ;; NOTE
  ;;  for `counsel--custom-company' function
  (advice-add #'delete-minibuffer-contents :around
              (lambda (fn)
                "To ignore `read-only' property."
                (let ((inhibit-read-only t))
                  (funcall fn))))

  (advice-add #'counsel-company :override #'counsel--custom-company)
  (advice-add #'company-indent-or-complete-common :override
              (lambda (arg)
                "Customize `company-indent-or-complete-common' use `counsel-company' instead of `company' overlay."
                (interactive "P")
                (cond
                 ((use-region-p)
                  (indent-region (region-beginning) (region-end)))
                 ((memq indent-line-function
                        '(indent-relative indent-relative-maybe))
                  (counsel--custom-company))
                 ((let ((old-point (point))
                        (old-tick (buffer-chars-modified-tick))
                        (tab-always-indent t))
                    (indent-for-tab-command arg)
                    (when (and (eq old-point (point))
                               (eq old-tick (buffer-chars-modified-tick)))
                      (counsel--custom-company))))))))

(use-package yasnippet
  :ensure t
  :hook ((eshell-mode . yas-minor-mode-on)
         (prog-mode   . yas-minor-mode-on))
  :config
  (with-eval-after-load "lsp-mode"
    (add-hook 'yas-before-expand-snippet-hook
              (lambda ()
                "Enable `lsp-signature-mode'"
                (when (and lsp-mode
                           (lsp-feature? "textDocument/signatureHelp")
                           (null lsp-signature-mode))
                  (ignore-errors
                    (setq lsp-signature-restart-enable t)
                    (lsp-signature-activate))))))

  (yas-load-directory "~/.emacs.d/snippets/" t))

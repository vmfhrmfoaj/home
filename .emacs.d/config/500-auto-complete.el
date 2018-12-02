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

(use-package helm-company
  :ensure t
  :after (company helm)
  :init
  (defun helm-company-plus ()
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
              :input (propertize company-prefix 'read-only t 'intangible t 'rear-nonsticky '(read-only intangible))
              :candidate-number-limit helm-company-candidate-number-limit))))

  (defun helm-company-complete-common ()
    "TODO"
    (interactive)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (helm-company-plus)
      (company--insert-candidate company-common)))

  (defun company-indent-or-helm-company ()
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
                  (evil-force-normal-state)))))
  ;; NOTE
  ;;  Turn company popup off completely.
  (remove-hook 'pre-command-hook 'company-pre-command)
  (remove-hook 'post-command-hook 'company-post-command)
  (advice-add #'company-mode-on :after
              (lambda ()
                (remove-hook 'pre-command-hook 'company-pre-command t)
                (remove-hook 'post-command-hook 'company-post-command t))))

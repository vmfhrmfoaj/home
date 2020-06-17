(use-package company-go
  :disabled t
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-go))))

(use-package go-eldoc
  :disabled t
  :ensure t
  :defer t
  :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :disabled t
  :ensure t
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode)
  :config
  (setq go-guru-hl-identifier-idle-time 0.3))

(use-package go-mode
  :disabled t
  :ensure t
  :defer t
  :config
  (setq gofmt-command "goimports")
  (let* ((go-path (concat home-dir "/Desktop/Go_Projects"))
         (go-bin-path (concat go-path "/bin")))
    (when (and (file-exists-p go-path)
               (file-exists-p go-bin-path))
      (setenv "GOPATH" go-path)
      (add-to-list 'process-environment (concat "GOPATH=" go-path))
      (setenv "PATH" (concat go-bin-path ":" (getenv "PATH")))
      (add-to-list 'exec-path go-bin-path)))

  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t)
              ;; (setq-local evil-lookup-func #'go-guru-)
              )))

(use-package company-go
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-go))))

(use-package go-eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-guru
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

  :config
  (setq go-guru-hl-identifier-idle-time 0.3))

(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t)
              ;; (setq-local evil-lookup-func #'go-guru-)
              ))

  :config
  (setq gofmt-command "goimports")
  (let* ((go-path (concat (getenv "HOME") "/Desktop/Go_Projects"))
         (go-bin-path (concat go-path "/bin")))
    (setenv "GOPATH" go-path)
    (add-to-list 'process-environment (concat "GOPATH=" go-path))
    (setenv "PATH" (concat go-bin-path ":" (getenv "PATH")))
    (add-to-list 'exec-path go-bin-path)))

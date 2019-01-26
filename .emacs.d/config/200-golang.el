(use-package company-go
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-go))))

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
              (setq-local font-lock-multiline t)))

  :config
  (setq gofmt-command "goimports")
  (let* ((go-path "~/.golang")
         (go-bin-path (concat go-path "/bin")))
    (setenv "GOPATH" go-path)
    (setenv "PATH" (concat go-bin-path ":" (getenv "PATH")))
    (add-to-list 'exec-path go-bin-path)))

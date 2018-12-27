(use-package elixir-mode
  ;; :ensure t
  :defer t
  :init
  (unless (package-installed-p 'elixir-mode)
    (quelpa '(elixir-mode :repo "vmfhrmfoaj/emacs-elixir" :fetcher github)))

  (add-hook 'elixir-mode-hook #'alchemist-mode))

(use-package alchemist
  :ensure t
  :defer t
  :diminish ""
  :commands (alchemist-goto-definition-at-point)
  :init
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'alchemist-help-search-at-point)
              (alchemist-server-start "dev")))

  :config
  (setq alchemist-hooks-compile-on-save t)
  (let ((opt-src (concat (getenv "HOME") "/Desktop/Open_Sources/otp"))
        (elixir-src (concat (getenv "HOME") "/Desktop/Open_Sources/elixir")))
    (and (file-exists-p opt-src)    (setq alchemist-goto-erlang-source-dir opt-src))
    (and (file-exists-p elixir-src) (setq alchemist-goto-elixir-source-dir elixir-src))))

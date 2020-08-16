;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package elixir-mode
  :disabled t
  :ensure t
  :defer t
  :config
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'elixir-mode-hook #'highlight-numbers-mode))

(use-package alchemist
  :disabled t
  :ensure t
  :defer t
  :diminish ""
  :commands (alchemist-goto-definition-at-point)
  :config
  (setq alchemist-hooks-compile-on-save t)
  (let ((opt-src (concat home-dir "/Desktop/Open_Sources/otp"))
        (elixir-src (concat home-dir "/Desktop/Open_Sources/elixir")))
    (and (file-exists-p opt-src)    (setq alchemist-goto-erlang-source-dir opt-src))
    (and (file-exists-p elixir-src) (setq alchemist-goto-elixir-source-dir elixir-src)))

  (add-hook 'alchemist-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'alchemist-help-search-at-point)
              (alchemist-server-start "dev"))))

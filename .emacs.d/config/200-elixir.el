(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :commands (alchemist-goto-definition-at-point)
  :config
  ;; NOTE:
  ;;  See, http://gernotklingler.com/blog/use-chroot-jail-software-development
  (setq alchemist-goto-elixir-source-dir (concat (getenv "HOME") "/Desktop/Open_Sources/elixir")
        alchemist-mix-command      "schroot -c elixir -- mix"
        alchemist-iex-program-name "schroot -c elixir -- iex"
        alchemist-execute-command  "schroot -c elixir -- elixir"
        alchemist-compile-command  "schroot -c elixir -- elixirc"))

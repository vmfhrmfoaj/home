(use-package elixir-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode))

(use-package alchemist
  :ensure t
  :defer t
  :diminish ""
  :commands (alchemist-goto-definition-at-point)
  :init
  (defvar alchemist-schroot-session nil)
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'alchemist-help-search-at-point)
              (alchemist-server-start "dev")))

  :config
  (setq alchemist-hooks-compile-on-save t)
  (unless alchemist-schroot-session
    (setq alchemist-schroot-session
          (->> (shell-command-to-string "schroot -c chroot:elixir --begin-session")
               (s-trim-right)
               (concat "session:")))
    ;; NOTE:
    ;;  See, http://gernotklingler.com/blog/use-chroot-jail-software-development
    (setq alchemist-mix-command      (concat "schroot -c " alchemist-schroot-session " -p -r -- mix")
          alchemist-iex-program-name (concat "schroot -c " alchemist-schroot-session " -p -r -- iex")
          alchemist-execute-command  (concat "schroot -c " alchemist-schroot-session " -p -r -- elixir")
          alchemist-compile-command  (concat "schroot -c " alchemist-schroot-session " -p -r -- elixirc"))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (ignore-errors
                  (dolist (proc alchemist-server-processes)
                    (kill-process (cdr proc))))
                (shell-command-to-string (concat "schroot -c " alchemist-schroot-session " --end-session")))))

  (let ((opt-src (concat (getenv "HOME") "/Desktop/Open_Sources/otp"))
        (elixir-src (concat (getenv "HOME") "/Desktop/Open_Sources/elixir")))
    (and (file-exists-p opt-src)    (setq alchemist-goto-erlang-source-dir opt-src))
    (and (file-exists-p elixir-src) (setq alchemist-goto-elixir-source-dir elixir-src))))

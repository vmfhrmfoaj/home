(use-package elixir-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)

  :config
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :unless '(:add sp-elixir-single-line-do-p)
                   :skip-match (lambda (ms mb me)
                                 (or (sp-elixir-skip-def-p ms mb me)
                                     (sp-elixir-skip-single-line-do-p ms mb me))))
    (sp-local-pair "def" "end"
                   :unless '(:add sp-elixir-single-line-do-p)
                   :skip-match 'sp-elixir-skip-single-line-do-p)
    (sp-local-pair "defp" "end"
                   :unless '(:add sp-elixir-single-line-do-p)
                   :skip-match 'sp-elixir-skip-single-line-do-p)
    (sp-local-pair "defmacro" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :unless '(sp-in-comment-p sp-in-string-p sp-elixir-single-line-do-p)
                   :skip-match 'sp-elixir-skip-single-line-do-p)
    (sp-local-pair "quote" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler-2)
                   :unless '(sp-in-comment-p sp-in-string-p sp-elixir-single-line-do-p)
                   :skip-match 'sp-elixir-skip-single-line-do-p)))

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

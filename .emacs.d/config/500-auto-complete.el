;; -*- lexical-binding: t; -*-

(eval-and-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode-on)
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-etags-ignore-case t
        company-echo-delay 0.2)

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (company--active-p)
                (company-cancel)))))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode-on))

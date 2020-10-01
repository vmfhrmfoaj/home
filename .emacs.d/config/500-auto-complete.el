;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode-on)
  :config
  (defun company-abort-and-insert-space ()
    "`company-abort' and insert a space."
    (interactive)
    (company-abort)
    (execute-kbd-macro (kbd "SPC")))

  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-minimum-length 1
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-etags-ignore-case t
        company-echo-delay 0.2)

  (add-hook 'company-after-completion-hook
            (lambda (_ignored)
              (when (timerp eldoc-timer)
                (cancel-timer eldoc-timer)
                (setq eldoc-timer nil))
              (run-at-time 0.1 nil (-partial #'call-interactively #'eldoc-refresh))))

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (company--active-p)
                (company-cancel)))))

(use-package helm-company
  :ensure t
  :after (company helm)
  :config
  (defun company-helm-candidates ()
    "TODO"
    (interactive)
    (when (and company-candidates
               company-point)
      (helm :sources 'helm-source-company
            :buffer  "*helm company*"
            :input (unless (s-blank-str? company-prefix)
                     (-> company-prefix
                         (regexp-quote)
                         (concat " ")
                         (propertize 'rear-nonsticky '(read-only intangible)
                                     'read-only t
                                     'intangible t))))
      (unless (looking-back "\\([,;]\\|\\s)\\)")
        (company-abort)
        (company-complete-common)))))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode-on))

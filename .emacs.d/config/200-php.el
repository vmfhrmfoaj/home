(use-package php-extras
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'php-extras)
    (quelpa '(php-extras :repo "arnested/php-extras" :fetcher github))
    (php-extras-generate-eldoc-1 t))

  (defvar php-doc-buffer-name "*PHP Doc*")

  (defun php-extras-doc ()
    (interactive)
    (-when-let (doc (php-extras-get-function-property (php-get-pattern) 'documentation))
      (pop-to-buffer (get-buffer-create php-doc-buffer-name))
      (evil-local-set-key 'normal (kbd "q") #'evil-delete-buffer)
      (kill-region (point-min) (point-max))
      (goto-char (point-min))
      (insert doc)
      (read-only-mode 1))))

(use-package php-mode
  :ensure t
  :defer t
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'php-extras-doc)
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
              (aggressive-indent-mode 1))))

(use-package company-php
  :ensure t
  :after php-mode
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-ac-php-backend))))

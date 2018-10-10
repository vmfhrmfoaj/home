(use-package company-php
  :ensure t
  :after php-mode
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-ac-php-backend))))

(use-package php-extras
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'php-extras)
    (quelpa '(php-extras :repo "arnested/php-extras" :fetcher github))
    (flet ((yes-or-no-p (&rest args) t))
      (php-extras-generate-eldoc)))

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
  :init
  (defvar php-code-beg-re "<\\?php[ \t\r\n]")
  (defvar php-code-end-re "[ \t\r\n]\\?>")

  (defun php-no-php-code-region ()
    (save-excursion
      (let ((beg (if (not (re-search-backward php-code-end-re nil t))
                     (point-min)
                   (re-search-forward php-code-end-re nil t)
                   (point)))
            (end (if (not (re-search-forward php-code-beg-re nil t))
                     (point-max)
                   (re-search-backward php-code-beg-re nil t)
                   (point))))
        (cons beg end))))

  (defun php-goto-end-of-php-code-block ()
    (unless (re-search-forward php-code-end-re nil t)
      (goto-char (point-max))))

  (defface php-unhilight-face
    '((t (:inherit (shadow default) :weight normal)))
    "TODO")

  (defun php-unhilight-no-php-code ()
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((block (php-no-php-code-region))
               (beg (car block))
               (end (cdr block)))
          (when (< beg end)
            (overlay-put (make-overlay beg end) 'face 'php-unhilight-face))
          (php-goto-end-of-php-code-block)))))

  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (php-unhilight-no-php-code)
              (setq-local evil-lookup-func #'php-extras-doc)
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
              (aggressive-indent-mode 1))))

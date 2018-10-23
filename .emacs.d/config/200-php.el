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
    (let ((fancy-narrow--beginning nil)
          (fancy-narrow--end nil))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let* ((block (php-no-php-code-region))
                 (beg (car block))
                 (end (cdr block)))
            (when (< beg end)
              (overlay-put (make-overlay beg end) 'face 'php-unhilight-face))
            (php-goto-end-of-php-code-block))))))

  :config
  (require 'psysh nil t)
  (add-hook 'php-mode-hook
            (lambda ()
              (php-unhilight-no-php-code)
              (setq-local evil-lookup-func #'php-extras-doc)
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
              (aggressive-indent-mode 1))))

(use-package psysh
  :ensure t
  :defer t
  :init
  (defvar psysh-schroot-session nil)

  (defun psysh-show ()
    (interactive)
    (-when-let (buf (get-buffer (concat "*" (car (psysh--detect-buffer)) "*")))
      (switch-to-buffer buf)))

  :config
  (advice-add #'psysh-restart :before #'psysh-show)

  (unless psysh-schroot-session
    (setq psysh-schroot-session
          (->> (shell-command-to-string "schroot -c chroot:php --begin-session")
               (s-trim-right)
               (concat "session:")))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (shell-command-to-string (concat "schroot -c " psysh-schroot-session " --end-session")))))
  ;; NOTE:
  ;;  See, http://gernotklingler.com/blog/use-chroot-jail-software-development
  (setq-default psysh-comint-buffer-process
                `("psysh" "schroot" nil "-c" ,psysh-schroot-session "-r" "--"
                  ,(concat (getenv "HOME") "/.composer/vendor/bin/psysh"))))

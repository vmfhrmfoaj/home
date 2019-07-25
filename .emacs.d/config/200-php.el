(use-package company-php
  :ensure t
  :after php-mode
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends #'company-ac-php-backend))))

(use-package php-extras
  :disabled t
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'php-extras)
    (quelpa '(php-extras :fetcher github :repo "arnested/php-extras"))
    (flet ((yes-or-no-p (&rest args) t))
      (php-extras-generate-eldoc)))

  (defvar php-doc-buffer-name "*PHP Doc*")

  (defn php-extras-doc--highlight (lines)
    (let ((php-code  (with-temp-buffer
                       (php-mode)
                       (insert (car lines))
                       (funcall font-lock-ensure-function (point-min) (point-max))
                       (buffer-string))))
      (->> lines
           (cdr)
           (cons "")
           (cons php-code))))

  (defn php-extras-doc ()
    (interactive)
    (-when-let (doc (->> (php-extras-get-function-property (php-get-pattern) 'documentation)
                         (s-split "\n")
                         (-drop 2)
                         (php-extras-doc--highlight)
                         (-reduce-from
                          (lambda (output line)
                            (if (s-blank? line)
                                (concat output "\n")
                              (concat output " " line)))
                          "")))
      (pop-to-buffer (get-buffer-create php-doc-buffer-name))
      (evil-local-set-key 'normal (kbd "q") #'evil-delete-buffer)
      (kill-region (point-min) (point-max))
      (goto-char (point-min))
      (insert doc)
      (toggle-truncate-lines 0)
      (read-only-mode 1)
      (goto-line 3))))

(use-package php-mode
  :ensure t
  :defer t
  :mode ("\\.php\\'" . php-mode)
  :init
  (defvar php-code-beg-re "<\\?php[ \t\r\n]")
  (defvar php-code-end-re "[ \t\r\n]\\?>")

  (defn php-goto-beg-of-php-code-block (&optional limit)
    (while (and (< (point-min) (point))
                (re-search-backward php-code-beg-re limit t)
                (sp-point-in-string-or-comment))))

  (defn php-goto-end-of-php-code-block (&optional limit)
    (let ((cur (point)))
      (while (and (< (point) (point-max))
                  (re-search-forward php-code-end-re limit t)
                  (sp-point-in-string-or-comment)))
      (when (= cur (point))
        (goto-char (point-max)))))

  (defn php-code-regions ()
    (let ((regions nil)
          (beg (point-min))
          (end (point-max))
          min)
      (save-excursion
        (goto-char beg)
        (while (not (= beg end))
          (setq end (progn (php-goto-end-of-php-code-block)     (point))
                beg (progn (php-goto-beg-of-php-code-block min) (point)))
          (when (not (= beg end))
            (add-to-list 'regions (cons beg end) t))
          (setq min end)
          (goto-char end))
        regions)))

  (defface php-unhilight-face
    '((t (:inherit (shadow default) :weight normal)))
    "TODO")

  (defn php-unhilight-no-php-code ()
    (let* ((fancy-narrow--beginning nil)
           (fancy-narrow--end nil)
           (php-code-regions (php-code-regions))
           (non-php-code-regions
            (->> (if (= (point-max) (cdr (-last-item php-code-regions)))
                     php-code-regions
                   (append php-code-regions (list (list (point-max)))))
                 (--reduce-from
                  (let ((beg (car it))
                        (end (cdr it)))
                    (append acc (list beg end)))
                  nil)
                 (-drop 1)
                 (-partition 2))))
      (dolist (region non-php-code-regions)
        (let ((beg (car  region))
              (end (cadr region)))
          (overlay-put (make-overlay beg end) 'face 'php-unhilight-face)))))

  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point))
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines))))

(use-package psysh
  :disabled t
  :ensure t
  :defer t
  :init
  (defn psysh-show ()
    (interactive)
    (-when-let (buf (get-buffer (concat "*" (car (psysh--detect-buffer)) "*")))
      (switch-to-buffer buf)))

  :config
  (advice-add #'psysh-restart :before #'psysh-show))

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package creole-mode
  :disabled t
  :ensure t
  :defer t
  :mode "\\.wikicreole\\'")

(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (defun markdown-open-link-at-point ()
    (interactive)
    (if-let ((url (and (member 'markdown-link-face (-list (get-text-property (point) 'face)))
                       (get-text-property (point) 'help-echo))))
        (browse-url--browser url)
      (message "Can't found url")))

  (setq markdown-fontify-code-blocks-natively t))

(use-package tex-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

  :config
  (with-eval-after-load "highlight-numbers"
    (puthash 'latex-mode
             (rx
              (and word-start
                   (? (* digit) ".")
                   (+ digit)
                   (or "pt" "mm" "cm" "in" "ex" "em" "mu" "sp")
                   symbol-end))
             highlight-numbers-modelist)))

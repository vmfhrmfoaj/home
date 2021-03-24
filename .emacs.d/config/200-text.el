;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package creole-mode
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

(use-package latex-mode
  :defer t
  :mode "\\.tex\\'")

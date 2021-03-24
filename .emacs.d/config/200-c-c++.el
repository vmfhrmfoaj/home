;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package cc-mode
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  (setq-default c-backslash-column 80
                c-backslash-max-column 120)

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'case-label '+)
              (unless (and (fboundp #'editorconfig-core-get-nearest-editorconfig)
                           (editorconfig-core-get-nearest-editorconfig default-directory))
                (setq c-basic-offset 4))
              (when (member major-mode '(c-mode c++-mode))
                (make-local-variable 'font-lock-extend-region-functions)
                (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
                (c-toggle-auto-newline -1)))
            :append)

  (add-hook 'java-mode-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'case-label '+)
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines))))

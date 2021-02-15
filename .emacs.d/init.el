(setq comp-deferred-compilation t
      comp-async-report-warnings-errors nil
      custom-file "~/.emacs.d/.custom.el"
      gc-cons-threshold (* 1024 1024 32)
      inhibit-startup-screen t)
(when (file-exists-p custom-file)
  (add-hook 'emacs-startup-hook (lambda () (load custom-file))))

(require 'package)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa"        . 10)
        ("gnu"          .  7)
        ("melpa-stable" .  5)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))
(use-package init-loader
  :ensure t
  :config
  (let ((file-name-handler-alist nil))
    (init-loader-load "~/.emacs.d/config")))

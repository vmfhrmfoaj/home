(when (version<= "28.0.50" emacs-version)
  ;; NOTE
  ;;  `define-obsolete-variable-alias' signature has been changed on version 28.
  ;;  To avoid error, revert the change for a while.
  (defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
    "For backward compatibility."
    (declare (doc-string 4))
    `(progn
       (defvaralias ,obsolete-name ,current-name ,docstring)
       ;; See Bug#4706.
       (dolist (prop '(saved-value saved-variable-comment))
         (and (get ,obsolete-name prop)
              (null (get ,current-name prop))
              (put ,current-name prop (get ,obsolete-name prop))))
       (make-obsolete-variable ,obsolete-name ,current-name ,when)))

  (defmacro define-obsolete-function-alias ( obsolete-name current-name
                                             &optional when docstring)
    "For backward compatibility."
    (declare (doc-string 4))
    `(progn
       (defalias ,obsolete-name ,current-name ,docstring)
       (make-obsolete ,obsolete-name ,current-name ,when))))

;; ---

(setq comp-async-report-warnings-errors nil
      comp-deferred-compilation t
      comp-speed 3
      native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation t
      native-comp-speed 3
      custom-file "~/.emacs.d/.custom.el"
      gc-cons-threshold (* 1024 1024 1024)
      inhibit-startup-screen t)

(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-archives
      '(("org"          . "https://orgmode.org/elpa/")
        ("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("org"          . 20)
        ("melpa"        . 10)
        ("gnu"          .  7)
        ("melpa-stable" .  5))
      package-native-compile t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))
(use-package init-loader
  :ensure t
  :config
  (let ((file-name-handler-alist nil))
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")
      (if (fboundp #'native-compile) (native-compile "~/.emacs.d/config/func.el")))
    (load-file "~/.emacs.d/config/func.el")
    (init-loader-load "~/.emacs.d/config")))

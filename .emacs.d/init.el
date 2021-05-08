(when (version<= "28.0.50" emacs-version)
  ;; NOTE
  ;;  `define-obsolete-variable-alias' signature has been changed on version 28.
  ;;  To avoid error, revert the change for a while.
  (defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
    "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:

  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")

This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
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
    "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
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
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa"        . 10)
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
    (init-loader-load "~/.emacs.d/config")))

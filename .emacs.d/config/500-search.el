;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package counsel
  :ensure t
  :defer t
  :config
  (defun counsel-rg-no-ignore-command ()
    (cond
     ((listp counsel-rg-base-command)
      (-concat (-drop-last 1 counsel-rg-base-command) '("--no-ignore" "%s")))
     ((stringp counsel-rg-base-command)
      (replace-in-string "%s" "--no-ignore %s" counsel-rg-base-command))
     (t '("rg" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "--no-ignore" "%s")))))

(use-package ivy
  :ensure t
  :defer t
  :config
  (defvar ivy-search-callers '(swiper counsel-rg counsel-projectile-rg))

  (defvar ivy-last-search-session nil)

  (defvar ivy-old-search-session nil)

  (defun ivy--set-ivy-last-search-session (alist-sym key val)
    (when (and (eq alist-sym 'ivy--sessions)
               (member key ivy-search-callers))
      (setq ivy-old-search-session ivy-last-search-session)
      (setq ivy-last-search-session key)))

  (defun ivy-resume-sarch ()
    (interactive)
    (if-let ((state (alist-get ivy-last-search-session ivy--sessions)))
        (if (eq 'swiper ivy-last-search-session)
            (if (and (stringp buffer-file-name)
                     (string= (file-truename buffer-file-name)
                              (or (-some-> state
                                    (ivy-state-extra-props)
                                    (plist-get :fname)
                                    (file-truename))
                                  "")))
                (ivy-resume 'swiper)
              (if (eq 'swiper ivy-old-search-session)
                  (message "There is no search result that can be resume")
                (ivy-resume ivy-old-search-session)))
          (ivy-resume ivy-last-search-session))
      (message "There is no search result that can be resume")))

  (advice-add #'ivy--alist-set :after #'ivy--set-ivy-last-search-session))

(use-package swiper
  :ensure t
  :defer t)

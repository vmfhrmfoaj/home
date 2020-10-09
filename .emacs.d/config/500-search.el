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
  (defvar ivy-search-callers '(swiper counsel-rg counsel-projectile-rg lsp-ivy-workspace-symbol))
  (defvar ivy-last-no-search-session nil)
  (defvar ivy-last-search-session nil)
  (defvar ivy-old-search-session nil)

  (defun ivy--set-ivy-session (alist-sym key val)
    (if (and (eq alist-sym 'ivy--sessions)
             (member key ivy-search-callers))
        (progn
          (setq ivy-old-search-session ivy-last-search-session)
          (setq ivy-last-search-session key))
      (setq ivy-last-no-search-session key)))

  (defun ivy-resume-non-search ()
    (interactive)
    (when ivy-last-no-search-session
      (ivy-resume ivy-last-no-search-session)))

  (defun ivy-resume-sarch ()
    (interactive)
    (if-let ((state (alist-get ivy-last-search-session ivy--sessions)))
        (if (eq 'swiper ivy-last-search-session)
            (let ((extra-props (ivy-state-extra-props state)))
              (if (and (stringp buffer-file-name)
                       (string= (file-truename buffer-file-name)
                                (or (-some-> extra-props
                                      (plist-get :fname)
                                      (file-truename))
                                    "")))
                  (let* ((data (plist-get extra-props :ivy-data))
                         (text (plist-get data :text))
                         (regex (ivy--regex text))
                         (last-item-line (-some->> (plist-get data :all-candidates)
                                           (--last (string-match-p regex it))
                                           (swiper--line-number)))
                         (need-to-update
                          (and last-item-line
                               (not (s-blank? text))
                               (save-excursion
                                 (goto-line last-item-line)
                                 (beginning-of-line)
                                 (print (list text last-item-line (buffer-substring-no-properties (point) (line-end-position))))
                                 (unless (re-search-forward regex (line-end-position) t) t)))))
                    (if need-to-update
                        (swiper text)
                      (ivy-resume 'swiper)))
                (if (eq 'swiper ivy-old-search-session)
                    (message "There is no search result that can be resume")
                  (ivy-resume ivy-old-search-session))))
          (ivy-resume ivy-last-search-session))
      (message "There is no search result that can be resume")))

  (advice-add #'ivy--alist-set :after #'ivy--set-ivy-session))

(use-package swiper
  :ensure t
  :defer t)

(use-package wgrep
  :ensure t
  :defer t)

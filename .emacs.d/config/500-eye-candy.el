(use-package all-the-icons
  :ensure t
  :defer t
  :init
  (defun all-the-icons-update-data (lst lst-key &rest kvs)
    "TODO"
    (let* ((key-vals (-partition 2 kvs))
           (keys (-map #'-first-item key-vals))
           (is-in-kvs? (-compose (-partial #'-contains? keys) #'car)))
      (when key-vals
        (-when-let (target (assoc lst-key (symbol-value lst)))
          (add-to-list lst
                       (->> target
                            (-drop 3)
                            (-partition-all 2)
                            (-remove is-in-kvs?)
                            (apply #'append (-take 3 target) kvs)))))))

  :config
  (setq all-the-icons-default-adjust -0.15)
  ;; for `neotree'
  (all-the-icons-update-data 'all-the-icons-dir-icon-alist "google[ _-]drive" :height 1.0)
  (all-the-icons-update-data 'all-the-icons-icon-alist "\\.DS_STORE$" :height 0.95 :v-adjust -0.1)
  (all-the-icons-update-data 'all-the-icons-icon-alist "\\.git" :v-adjust 0)
  ;; for `mode-line'
  (all-the-icons-update-data 'all-the-icons-mode-icon-alist 'rust-mode :v-adjust 0)
  (all-the-icons-update-data 'all-the-icons-mode-icon-alist 'java-mode :v-adjust 0.05 :height 1.1)
  (dolist (mode '(magit-status-mode
                  magit-log-mode))
    (all-the-icons-update-data 'all-the-icons-mode-icon-alist mode :v-adjust 0))
  (dolist (mode '(emacs-lisp-mode
                  inferior-emacs-lisp-mode
                  fundamental-mode
                  special-mode))
    (all-the-icons-update-data 'all-the-icons-mode-icon-alist mode :v-adjust -0.1))
  (advice-add #'all-the-icons-icon-for-dir :filter-args
              (lambda (args)
                (let* ((dir (car args))
                       (dir (s-chop-suffix "/" dir)))
                  (push (if (string-empty-p dir)
                            "/"
                          dir)
                        (-drop 1 args))))))

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (advice-add #'adob--never-dim-p :before-until #'helm-bufferp)
  (auto-dim-other-buffers-mode))

(use-package diminish
  :ensure t
  :config
  (with-eval-after-load "auto-dim-other-buffers") (diminish 'auto-dim-other-buffers-mode "")
  (with-eval-after-load "abbrev"                  (diminish 'abbrev-mode                 ""))
  (with-eval-after-load "aggressive-indent"       (diminish 'aggressive-indent-mode      ""))
  (with-eval-after-load "autorevert"              (diminish 'auto-revert-mode            ""))
  (with-eval-after-load "company"                 (diminish 'company-mode                ""))
  (with-eval-after-load "eldoc"                   (diminish 'eldoc-mode                  ""))
  (with-eval-after-load "elisp-slime-nav"         (diminish 'elisp-slime-nav-mode        ""))
  (with-eval-after-load "evil-goggles"            (diminish 'evil-goggles-mode           ""))
  (with-eval-after-load "evil-org"                (diminish 'evil-org-mode               ""))
  (with-eval-after-load "git-gutter+"             (diminish 'git-gutter+-mode            ""))
  (with-eval-after-load "helm"                    (diminish 'helm-mode                   ""))
  (with-eval-after-load "highlight-parentheses"   (diminish 'highlight-parentheses-mode  ""))
  (with-eval-after-load "highlight-symbol"        (diminish 'highlight-symbol-mode       ""))
  (with-eval-after-load "linum-relative"          (diminish 'linum-relative-mode         ""))
  (with-eval-after-load "magit-svn"               (diminish 'magit-svn-mode              ""))
  (with-eval-after-load "org-indent"              (diminish 'org-indent-mode             ""))
  (with-eval-after-load "simple"                  (diminish 'auto-fill-function          ""))
  (with-eval-after-load "smartparens"             (diminish 'smartparens-mode            ""))
  (with-eval-after-load "subword"                 (diminish 'subword-mode                ""))
  (with-eval-after-load "undo-tree"               (diminish 'undo-tree-mode              ""))
  (with-eval-after-load "vi-tilde-fringe"         (diminish 'vi-tilde-fringe-mode        ""))
  (with-eval-after-load "view"                    (diminish 'view-mode                   ""))
  (with-eval-after-load "which-key"               (diminish 'which-key-mode              ""))
  (with-eval-after-load "editorconfig"            (diminish 'editorconfig-mode           ""))
  (with-eval-after-load "zoom"                    (diminish 'zoom-mode                   "")))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package fancy-narrow
  :ensure t
  :defer t
  :config
  (with-eval-after-load "helm-swoop"
    (advice-add #'fancy-narrow-to-region :after #'helm-swoop--clear-cache-hard)
    (advice-add #'fancy-widen :after #'helm-swoop--clear-cache-hard))
  (advice-add #'save-buffer :around
              (lambda (fn &optional arg)
                "wrap `save-buffer' to run without `fancy-narrow'."
                (let (fancy-narrow--beginning fancy-narrow--end)
                  (funcall fn arg))))
  (advice-add #'jit-lock-function :around
              (lambda (fn beg)
                "wrap `jit-lock-function' to run without `fancy-narrow'."
                (let (fancy-narrow--beginning fancy-narrow--end)
                  (funcall fn beg)))))

(use-package focus
  :ensure t
  :defer t
  :init
  (defun focus--org-thing ()
    "TODO"
    (ignore-errors
      (if focus-mode-org-thing-lock
          (cons 0 0)
        (save-excursion
          (let ((beg (progn
                       (outline-previous-heading)
                       (point)))
                (end   (progn
                         (outline-next-visible-heading 1)
                         (beginning-of-line)
                         (point))))
            (cons beg end))))))

  (defun focus--tex-thing ()
    "TODO"
    (ignore-errors
      (let* ((regx  (concat "^\\(?:[[:cntrl:]]\\)*$"))
             (beg (save-excursion
                    (backward-char)
                    (re-search-backward regx nil t)
                    (point)))
             (end   (save-excursion
                      (forward-char)
                      (re-search-forward regx nil t)
                      (point))))
        (cons beg end))))

  (defun focus--text-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (backward-paragraph)
                     (point)))
              (end (progn
                     (forward-paragraph)
                     (point))))
          (cons beg end)))))

  (defun focus--list+-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (cond ((sp-point-in-string)
                              (save-match-data
                                (re-search-backward "[^\\]\""))
                              (forward-char))
                             ((sp-point-in-comment)
                              (beginning-of-line)))
                       (backward-up-list 2))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (defun focus--lisp-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (while (progn
                                (backward-up-list 1 t t)
                                (not (looking-at-p "(\\(\\(lexical-\\)?let\\*?\\|lambda\\|defun\\|defmacro\\)\\_>")))))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (defun focus--clojure-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((beg (progn
                     (ignore-errors
                       (while (progn
                                (backward-up-list 1 t t)
                                (not (looking-at-p "(\\([-0-9A-Za-z]+/\\)?\\(let\\|loop\\|doseq\\|fn\\|def[a-z]*\\)\\_>")))))
                     (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons beg end)))))

  (defvar focus--exclude-modes '(term-mode)
    "TODO")

  (defun focus--enable ()
    "TODO"
    (unless (apply #'derived-mode-p focus--exclude-modes)
      (focus-mode 1)
      (let ((focus-update-idle-time nil))
        (focus-move-focus))))

  (defun focus--disable ()
    "TODO"
    (focus-mode 0))

  (put 'org          'bounds-of-thing-at-point #'focus--org-thing)
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--tex-thing)
  (put 'sentence+    'bounds-of-thing-at-point #'focus--text-thing)
  (put 'list+        'bounds-of-thing-at-point #'focus--list+-thing)
  (put 'lisp         'bounds-of-thing-at-point #'focus--lisp-thing)
  (put 'clojure      'bounds-of-thing-at-point #'focus--clojure-thing)
  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook #'focus--disable)

  :config
  (setq focus-update-idle-time 0.2
        focus--update-timer nil)
  (make-local-variable 'focus--update-timer)
  (add-to-list 'focus-mode-to-thing '(clojure-mode . clojure))
  (add-to-list 'focus-mode-to-thing '(cider-repl-mode . list+))
  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp))
  (add-to-list 'focus-mode-to-thing '(org-mode . org))
  (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
  (add-to-list 'focus-mode-to-thing '(text-mode . sentence+))
  (advice-add #'focus-terminate :after
              (lambda ()
                "Clear `focus--update-timer`."
                (when focus--update-timer
                  (cancel-timer focus--update-timer)
                  (setq focus--update-timer nil))))
  (advice-add #'focus-move-focus :around
              (lambda (fn)
                "Delay to update `focus-pre-overlay' and `focus-post-overlay'."
                (when focus--update-timer
                  (cancel-timer focus--update-timer))
                (if (not focus-update-idle-time)
                    (funcall fn)
                  (setq focus--update-timer
                        (run-with-idle-timer
                         focus-update-idle-time
                         nil
                         (lambda (fn)
                           (setq focus--update-timer nil)
                           (condition-case nil
                               (funcall fn)
                             (error (progn
                                      (focus-terminate)
                                      (focus-init)
                                      (funcall fn)))))
                         fn))))))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol
  :ensure t
  :init
  (defun highlight-symbol--custom-update-timer (value)
    "TODO"
    (when highlight-symbol-timer
      (cancel-timer highlight-symbol-timer))
    (setq highlight-symbol-timer
          (run-with-idle-timer (min 0.1 value) nil #'highlight-symbol-temp-highlight)))

  (defun highlight-symbol-mode--custom-post-command ()
    "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
    (if (eq this-command 'highlight-symbol-jump)
        (when highlight-symbol-on-navigation-p
          (highlight-symbol-temp-highlight))
      (highlight-symbol--custom-update-timer highlight-symbol-idle-delay)))

  (defvar highlight-symbol-ignore-face-list
    '(font-lock-doc-face font-lock-comment-face font-lock-string-face php-string)
    "TODO")

  (defun highlight-symbol--custom-get-symbol ()
    "Return a regular expression identifying the symbol at point.
This is customized for the normal state of `evil-mode'."
    (-when-let (bound (bounds-of-thing-at-point 'symbol))
      (let ((beg (car bound))
            (end (cdr bound)))
        (when (< (point) end)
          (let ((symbol (buffer-substring beg end)))
            (unless (or (-intersection (-list (get-text-property beg 'face)) highlight-symbol-ignore-face-list)
                        (--some (string-match-p it symbol) highlight-symbol-ignore-list))
              (concat (car highlight-symbol-border-pattern)
                      (regexp-quote symbol)
                      (cdr highlight-symbol-border-pattern))))))))

  (defvar highlight-symbol-enable-modes
    '(prog-mode)
    "TODO")

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (-some #'derived-mode-p highlight-symbol-enable-modes)
                (highlight-symbol-mode  1))))

  (add-hook 'evil-normal-state-exit-hook
            (lambda ()
              (highlight-symbol-mode -1)))

  :config
  (setq highlight-symbol-idle-delay 0.2)
  (advice-add #'highlight-symbol-update-timer :override #'highlight-symbol--custom-update-timer)
  (advice-add #'highlight-symbol-mode-post-command :override #'highlight-symbol-mode--custom-post-command)
  (advice-add #'highlight-symbol-get-symbol :override #'highlight-symbol--custom-get-symbol))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :init
  (defun hl-todo--setup-custom ()
    "TODO"
    (font-lock-remove-keywords nil hl-todo--keywords)
    (setq hl-todo--keywords
          (list (list (caar hl-todo--keywords)
                      `(1 (hl-todo--get-face) prepend))))
    (font-lock-add-keywords nil hl-todo--keywords t))

  :config
  (advice-add #'hl-todo--get-face :filter-return #'list)
  (advice-add #'hl-todo--setup :after #'hl-todo--setup-custom)
  (global-hl-todo-mode 1))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)

  :config
  (setq org-bullets-bullet-list '("■" "□" "●" "○" "◌")))

(use-package powerline
  :ensure t
  :config
  (setq powerline-height (+ (frame-char-height) line-spacing)))

(use-package rainbow-delimiters
  :disabled t
  :ensure t
  :defer t
  :init
  (defvar rainbow-delimiters--prefix-str (concat "@" "?" "#" "_" "'" "`")
    "TODO")

  (defun rainbow-delimiters--apply-color-for-fira-code (loc depth match)
    "TODO"
    (-when-let (face (funcall rainbow-delimiters-pick-face-function depth match loc))
      (font-lock-prepend-text-property (save-excursion
                                         (goto-char loc)
                                         (when (looking-at-p "\\s(")
                                           (skip-chars-backward rainbow-delimiters--prefix-str))
                                         (point))
                                       (1+ loc)
                                       'face face)))

  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  :config
  (when (eq 'darwin system-type)
    (advice-add #'rainbow-delimiters--apply-color :override
                #'rainbow-delimiters--apply-color-for-fira-code)))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-window-numbers-unicode t)
  (advice-add #'spaceline--unicode-number :override
              (lambda (str)
                (apply #'propertize
                 (cond
                  ((string=  "1" str) "⑴")
                  ((string=  "2" str) "⑵")
                  ((string=  "3" str) "⑶")
                  ((string=  "4" str) "⑷")
                  ((string=  "5" str) "⑸")
                  ((string=  "6" str) "⑹")
                  ((string=  "7" str) "⑺")
                  ((string=  "8" str) "⑻")
                  ((string=  "9" str) "⑼")
                  ((string= "10" str) "⑽")
                  ((string= "11" str) "⑾")
                  ((string= "12" str) "⑿")
                  ((string= "13" str) "⒀")
                  ((string= "14" str) "⒁")
                  ((string= "15" str) "⒂")
                  ((string= "16" str) "⒃")
                  ((string= "17" str) "⒄")
                  ((string= "18" str) "⒅")
                  ((string= "19" str) "⒆")
                  ((string= "20" str) "⒇")
                  (t "⒳"))
                 (cond
                  ((or (string-equal "gnome-macbookair" hostname)
                       (string-equal "gnome-imac" hostname))
                   '(display (raise 0.05) face (:weight ultrabold :inherit)))
                  (t
                   '(face (:height 1.15 :weight ultrabold :inherit)))))))
  (spaceline-emacs-theme)
  (spaceline-helm-mode))

(use-package vi-tilde-fringe
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'vi-tilde-fringe)
    (quelpa '(vi-tilde-fringe :repo "syl20bnr/vi-tilde-fringe" :fetcher github)))
  (global-vi-tilde-fringe-mode))

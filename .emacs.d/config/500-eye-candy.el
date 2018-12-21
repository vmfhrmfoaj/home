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
  (all-the-icons-update-data 'all-the-icons-dir-icon-alist "google[ _-]drive" :height 1.0)
  (all-the-icons-update-data 'all-the-icons-icon-alist "\\.DS_STORE$" :height 0.95 :v-adjust -0.1)
  (advice-add #'all-the-icons-icon-for-dir :filter-args
              (lambda (args)
                (let* ((dir (car args))
                       (dir (s-chop-suffix "/" dir)))
                  (push (if (string-empty-p dir)
                            "/"
                          dir)
                        (-drop 1 args))))))

(use-package auto-dim-other-buffers
  :disabled t
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

  (defun focus--text-thing ()
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

  (defalias 'focus-move-focus-internal 'focus-move-focus)

  (defun focus--enable ()
    "TODO"
    (unless (apply #'derived-mode-p focus--exclude-modes)
      (focus-mode 1)
      (focus-move-focus-internal)))

  (defun focus--disable ()
    "TODO"
    (focus-mode 0))

  (put 'org          'bounds-of-thing-at-point #'focus--org-thing)
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--text-thing)
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
                  fn)))))

(use-package highlight-parentheses
  :disabled t
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-line
  :disabled t
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
  :init
  (defvar powerline-buffer-id-max 40
    "TODO")

  (defun powerline-ellipsis-buffer-id (&optional buf-id)
    "TODO"
    (let ((buf-id (or buf-id (powerline-buffer-id))))
      (if (<= (length buf-id) powerline-buffer-id-max)
          buf-id
        (let ((substr (substring buf-id 0 powerline-buffer-id-max))
              (face (get-text-property 1 'face buf-id)))
          (concat substr (propertize "..." 'face face))))))

  (defun powerline-vim+-theme ()
    "Setup a Vim-like mode-line."
    (interactive)
    (setq-default
     mode-line-format
		 '("%e"
		   (:eval
		    (let* ((active (powerline-selected-window-active))
			         (mode-line (if active 'mode-line 'mode-line-inactive))
               (face0 (if active 'powerline-active0 'powerline-inactive0))
			         (lhs (list (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				                  (powerline-raw "[" face0 'l)
				                  (powerline-major-mode face0)
				                  (powerline-process face0)
				                  (powerline-raw "]" face0)
				                  (when (buffer-modified-p)
				                    (powerline-raw "[+]" face0))
				                  (when buffer-read-only
				                    (powerline-raw "[RO]" face0))
                          (concat (powerline-raw "[" face0 'l)
                                  (powerline-raw (if (not current-input-method)
                                                     "EN"
                                                   (->> input-method-alist
                                                        (assoc current-input-method)
                                                        (nth 3)))
                                                 face0)
                                  (powerline-raw "]" face0))
				                  (powerline-raw "[" face0 'l)
				                  (powerline-minor-modes face0)
				                  (powerline-raw "%n" face0)
				                  (powerline-raw "]" face0)
				                  (when (and vc-mode buffer-file-name)
				                    (let ((backend (vc-backend buffer-file-name)))
					                    (when backend
					                      (concat (powerline-raw "[" face0 'l)
						                            (powerline-raw (format "%s:%s(%.5s)" backend
                                                               (or (-some-> vc-mode
                                                                            (split-string "[-:@]")
                                                                            (rest)
                                                                            (-some->> (-interpose "-")
                                                                                      (apply #'concat)))
                                                                   "-")
                                                               (vc-working-revision buffer-file-name backend))
                                                       face0)
						                            (powerline-raw "]" face0)))))))
			         (rhs (list (powerline-raw '(10 "%i") face0)
				                  (powerline-raw global-mode-string face0 'r)
				                  (powerline-raw "%l," face0 'l)
				                  (powerline-raw (format-mode-line '(10 "%c")) face0)
				                  (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) face0 'r)
				                  (powerline-fill face0 0))))
		      (concat (powerline-render lhs)
			            (powerline-fill face0 (powerline-width rhs))
			            (powerline-render rhs)))))))

  :config
  (advice-add #'powerline-buffer-id :filter-return #'powerline-ellipsis-buffer-id)
  (powerline-vim+-theme))

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

(use-package vi-tilde-fringe
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'vi-tilde-fringe)
    (quelpa '(vi-tilde-fringe :repo "syl20bnr/vi-tilde-fringe" :fetcher github)))
  (global-vi-tilde-fringe-mode))

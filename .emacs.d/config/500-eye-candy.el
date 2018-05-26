(use-package all-the-icons
  :ensure t
  :defer t)

(use-package auto-dim-other-buffers
  :disabled t
  :ensure t
  :config
  (auto-dim-other-buffers-mode 1))

(use-package diminish
  :ensure t
  :config
  (with-eval-after-load "abbrev"                (diminish 'abbrev-mode                "Ⓐ"))
  (with-eval-after-load "aggressive-indent"     (diminish 'aggressive-indent-mode     "Ⓐ"))
  (with-eval-after-load "autorevert"            (diminish 'auto-revert-mode           "Ⓐ"))
  (with-eval-after-load "company"               (diminish 'company-mode               "Ⓒ"))
  (with-eval-after-load "eldoc"                 (diminish 'eldoc-mode                 "Ⓔ"))
  (with-eval-after-load "elisp-slime-nav"       (diminish 'elisp-slime-nav-mode       "Ⓔ"))
  (with-eval-after-load "evil-goggles"          (diminish 'evil-goggles-mode          "Ⓔ"))
  (with-eval-after-load "evil-org"              (diminish 'evil-org-mode              "Ⓔ"))
  (with-eval-after-load "git-gutter+"           (diminish 'git-gutter+-mode           "Ⓖ"))
  (with-eval-after-load "helm"                  (diminish 'helm-mode                  "Ⓗ"))
  (with-eval-after-load "highlight-parentheses" (diminish 'highlight-parentheses-mode "Ⓗ"))
  (with-eval-after-load "magit-svn"             (diminish 'magit-svn-mode             "Ⓜ"))
  (with-eval-after-load "org-indent"            (diminish 'org-indent-mode            "Ⓞ"))
  (with-eval-after-load "simple"                (diminish 'auto-fill-function         "Ⓐ"))
  (with-eval-after-load "smartparens"           (diminish 'smartparens-mode           "Ⓢ"))
  (with-eval-after-load "subword"               (diminish 'subword-mode               "Ⓢ"))
  (with-eval-after-load "undo-tree"             (diminish 'undo-tree-mode             "Ⓤ"))
  (with-eval-after-load "vi-tilde-fringe"       (diminish 'vi-tilde-fringe-mode       "Ⓥ"))
  (with-eval-after-load "view"                  (diminish 'view-mode                  "Ⓥ"))
  (with-eval-after-load "which-key"             (diminish 'which-key-mode             "Ⓦ"))
  (with-eval-after-load "zoom"                  (diminish 'zoom-mode                  "Ⓩ")))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package fancy-narrow
  :ensure t
  :defer t
  :commands (fancy-narrow-to-defun
             fancy-narrow-to-page
             fancy-narrow-to-region
             fancy-widen)

  :config
  (with-eval-after-load "helm-swoop"
    (advice-add #'fancy-narrow-to-region :after #'helm-swoop--clear-cache)
    (advice-add #'fancy-widen :after #'helm-swoop--clear-cache))
  (advice-add #'save-buffer :around
              (lambda (fn &optional arg)
                (let (fancy-narrow--beginning fancy-narrow--end)
                  (funcall fn arg))))
  (advice-add #'jit-lock-function :around
              (byte-compile
               (lambda (fn start)
                 (let (fancy-narrow--beginning fancy-narrow--end)
                   (funcall fn start))))))

(use-package focus
  :ensure t
  :init
  (defun focus--org-thing ()
    "TODO"
    (ignore-errors
      (if focus-mode-org-thing-lock
          (cons 0 0)
        (save-excursion
          (let ((start (progn
                         (outline-previous-heading)
                         (point)))
                (end   (progn
                         (outline-next-visible-heading 1)
                         (beginning-of-line)
                         (point))))
            (cons start end))))))

  (defun focus--text-thing ()
    "TODO"
    (ignore-errors
      (let* ((regx  (concat "^\\(?:[[:cntrl:]]\\)*$"))
             (start (save-excursion
                      (backward-char)
                      (re-search-backward regx nil t)
                      (point)))
             (end   (save-excursion
                      (forward-char)
                      (re-search-forward regx nil t)
                      (point))))
        (cons start end))))

  (defun focus--list+-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((start (progn
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
          (cons start end)))))

  (defun focus--lisp-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((start (progn
                       (ignore-errors
                         (while (progn
                                  (backward-up-list 1 t t)
                                  (not (looking-at-p "(\\(\\(lexical-\\)?let\\*?\\|lambda\\|defun\\|defmacro\\)\\_>")))))
                       (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons start end)))))

  (defun focus--clojure-thing ()
    "TODO"
    (ignore-errors
      (save-excursion
        (let ((start (progn
                       (ignore-errors
                         (while (progn
                                  (backward-up-list 1 t t)
                                  (not (looking-at-p "(\\([-0-9A-Za-z]+/\\)?\\(let\\|loop\\|fn\\|def[a-z]*\\)\\_>")))))
                       (point)))
              (end (progn
                     (forward-list)
                     (point))))
          (cons start end)))))

  (defvar focus--exclude-modes '(term-mode)
    "TODO")

  (defun focus--enable ()
    "TODO"
    (unless (apply #'derived-mode-p focus--exclude-modes)
      (focus-mode 1)))

  (defun focus--disable ()
    "TODO"
    (focus-mode 0))

  :config
  (put 'org          'bounds-of-thing-at-point #'focus--org-thing)
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--text-thing)
  (put 'list+        'bounds-of-thing-at-point #'focus--list+-thing)
  (put 'lisp         'bounds-of-thing-at-point #'focus--lisp-thing)
  (put 'clojure      'bounds-of-thing-at-point #'focus--clojure-thing)
  (add-to-list 'focus-mode-to-thing '(clojure-mode . clojure))
  (add-to-list 'focus-mode-to-thing '(cider-repl-mode . list+))
  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp))
  (add-to-list 'focus-mode-to-thing '(org-mode . org))
  (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook #'focus--disable)
  (advice-add #'focus-move-focus :around
              (byte-compile
               (lambda (of)
                 (condition-case nil
                     (funcall of)
                   (error (progn
                            (focus-terminate)
                            (focus-init)
                            (funcall of))))))))

(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :init
  (defun hl-todo--re-setup ()
    "TODO"
    (font-lock-remove-keywords nil hl-todo--keywords)
    (setq hl-todo--keywords
          (list (list (caar hl-todo--keywords)
                      `(1 (hl-todo--get-face) prepend))))
    (font-lock-add-keywords nil hl-todo--keywords t))

  :config
  (advice-add #'hl-todo--get-face :filter-return #'list)
  (advice-add #'hl-todo--setup :after #'hl-todo--re-setup)
  (global-hl-todo-mode 1))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package powerline
  :ensure t
  :init
  (defvar powerline-buffer-id-max 40
    "TODO")

  (defun powerline-ellipsis-buffer-id (&optional buf-id)
    "TODO"
    (let ((buf-id (or buf-id (powerline-buffer-id)))
          (face (get-text-property 1 'face buf-id)))
      (if (< powerline-buffer-id-max (length buf-id))
          (concat (substring buf-id 0 powerline-buffer-id-max)
                  (propertize "..." 'face face))
        buf-id)))

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
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  :after evil
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'vi-tilde-fringe nil 'noerror)
    (quelpa '(vi-tilde-fringe :repo "syl20bnr/vi-tilde-fringe" :fetcher github)))

  :config
  (global-vi-tilde-fringe-mode))

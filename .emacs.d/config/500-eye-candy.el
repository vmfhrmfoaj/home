(use-package auto-dim-other-buffers
  :ensure t
  :defer t
  :init
  (defn auto-dim-other-initial-setup ()
    (remove-hook 'window-configuration-change-hook #'auto-dim-other-initial-setup)
    (require 'auto-dim-other-buffers))

  (add-hook 'after-init-hook
            (lambda () (add-hook 'window-configuration-change-hook #'auto-dim-other-initial-setup)))

  :config
  (advice-add #'adob--never-dim-p :before-until #'helm-bufferp)
  (with-eval-after-load "helm-occur"
    (advice-add #'adob--never-dim-p :before-until #'helm-occur-current-bufferp))

  (make-thread #'auto-dim-other-buffers-mode))

(use-package diminish
  :ensure t
  :config
  (with-eval-after-load "auto-dim-other-buffers"  (diminish 'auto-dim-other-buffers-mode ""))
  (with-eval-after-load "abbrev"                  (diminish 'abbrev-mode                 ""))
  (with-eval-after-load "autorevert"              (diminish 'auto-revert-mode            ""))
  (with-eval-after-load "company"                 (diminish 'company-mode                ""))
  (with-eval-after-load "editorconfig"            (diminish 'editorconfig-mode           ""))
  (with-eval-after-load "eldoc"                   (diminish 'eldoc-mode                  ""))
  (with-eval-after-load "elisp-slime-nav"         (diminish 'elisp-slime-nav-mode        ""))
  (with-eval-after-load "evil-goggles"            (diminish 'evil-goggles-mode           ""))
  (with-eval-after-load "flycheck"                (diminish 'flycheck-mode               ""))
  (with-eval-after-load "git-gutter+"             (diminish 'git-gutter+-mode            ""))
  (with-eval-after-load "helm"                    (diminish 'helm-mode                   ""))
  (with-eval-after-load "highlight-parentheses"   (diminish 'highlight-parentheses-mode  ""))
  (with-eval-after-load "linum-relative"          (diminish 'linum-relative-mode         ""))
  (with-eval-after-load "magit-blame"             (diminish 'magit-blame-mode            ""))
  (with-eval-after-load "magit-svn"               (diminish 'magit-svn-mode              ""))
  (with-eval-after-load "simple"                  (diminish 'auto-fill-function          ""))
  (with-eval-after-load "smartparens"             (diminish 'smartparens-mode            ""))
  (with-eval-after-load "subword"                 (diminish 'subword-mode                ""))
  (with-eval-after-load "undo-tree"               (diminish 'undo-tree-mode              ""))
  (with-eval-after-load "vi-tilde-fringe"         (diminish 'vi-tilde-fringe-mode        ""))
  (with-eval-after-load "view"                    (diminish 'view-mode                   ""))
  (with-eval-after-load "which-key"               (diminish 'which-key-mode              ""))
  (with-eval-after-load "yasnippet"               (diminish 'yas-minor-mode              ""))
  (with-eval-after-load "zoom"                    (diminish 'zoom-mode                   "")))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (setq evil-goggles-duration 0.2)

  (add-hook 'minibuffer-setup-hook
            (byte-compile
             (lambda ()
               (setq-local evil-goggles-duration 0.001))))

  (evil-goggles-use-diff-faces)
  (make-thread #'evil-goggles-mode))

(use-package fancy-narrow
  :ensure t
  :defer t
  :config
  (plist-put fancy-narrow-properties 'fontified t)

  (with-eval-after-load "helm-occur"
    (fancy-narrow--advise-function #'helm-occur))

  (advice-add #'fancy-narrow-to-region :after (lambda (&rest _) "To cancel a selection." (keyboard-quit)))
  (let ((f (byte-compile
            (lambda (fn &rest args)
              "wrap a function to run without `fancy-narrow'."
              (let (fancy-narrow--beginning fancy-narrow--end)
                (apply fn args))))))
    (advice-add #'save-buffer :around f)
    (advice-add #'jit-lock-function :around f)
    ;; NOTE
    ;;  For avoiding to conflict between `evil-forward-nearest' and `fancy-narrow'.
    ;;  But this is not mean that allow to go out of boundary of `fancy-narrow'.
    (advice-add #'evil-forward-nearest :around f)
    (eval-after-load "cc-mode"
      `(advice-add #'c-after-change :around ,f))
    (eval-after-load "lsp"
      `(advice-add #'lsp-on-change :around ,f))))

(use-package focus
  :ensure t
  :defer t
  :commands (focus-init focus-terminate)
  :init
  (defvar focus--exclude-modes '(term-mode)
    "TODO")

  (defn focus--enable ()
    "TODO"
    (unless (or (apply #'derived-mode-p focus--exclude-modes)
                (bound-and-true-p helm-alive-p)
                (minibufferp))
      (focus-init)
      (let ((focus-update-idle-time nil))
        (focus-move-focus))))

  (defn focus--disable ()
    "TODO"
    (focus-terminate))

  (add-hook 'evil-insert-state-entry-hook #'focus--enable)
  (add-hook 'evil-insert-state-exit-hook  #'focus--disable)

  :config
  (defn focus--tex-thing ()
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

  (defn focus--text-thing ()
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

  (defn focus--list+-thing ()
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

  (defn focus--lisp-thing ()
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

  (with-eval-after-load "clojure-mode"
    (defn focus--clojure-thing ()
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

    (put 'clojure 'bounds-of-thing-at-point #'focus--clojure-thing)
    (put 'list+   'bounds-of-thing-at-point #'focus--list+-thing)
    (add-to-list 'focus-mode-to-thing '(clojure-mode    . clojure))
    (add-to-list 'focus-mode-to-thing '(cider-repl-mode . list+)))

  (let ((form
         '(progn
            (unless (fboundp 'focus--c-style-thing)
              (defn focus--c-style-thing ()
                "TODO"
                (ignore-errors
                  (save-excursion
                    (let (end)
                      (while (progn
                               (backward-up-list 1 t t)
                               (not (char-equal ?{ (char-after)))))
                      (save-excursion
                        (forward-list)
                        (end-of-line)
                        (setq end (point)))
                      (beginning-of-line)
                      (unless (looking-at-p "\\s-*}")
                        (let ((keep-going t))
                          (while (and (not (bobp))
                                      (not (looking-at-p "[[:space:]]*$"))
                                      (not (looking-at-p "\\s-*//"))
                                      (not (looking-at-p ".*,(\\s-*//.*)?\\s-*$"))
                                      (not (looking-at-p ".*;(\\s-*//.*)?\\s-*$"))
                                      keep-going)
                            (forward-line -1)
                            (beginning-of-line)
                            (when (looking-at-p ".*{\\s-*$")
                              (setq keep-going nil)))))
                      (skip-chars-forward " \t}" )
                      (cons (point) end)))))
              (put 'c-style 'bounds-of-thing-at-point #'focus--c-style-thing)))))
    (eval-after-load "cc-mode"
      (append form '((add-to-list 'focus-mode-to-thing '(c-mode    . c-style))
                     (add-to-list 'focus-mode-to-thing '(c++-mode  . c-style))
                     (add-to-list 'focus-mode-to-thing '(java-mode . c-style)))))
    (eval-after-load "rust-mode"
      (append form '((add-to-list 'focus-mode-to-thing '(rust-mode . c-style))))))

  (defvar-local focus--update-timer nil
    "TODO")

  (defn focus--clear-update-timer ()
    "Clear `focus--update-timer`."
    (when focus--update-timer
      (cancel-timer focus--update-timer)
      (setq focus--update-timer nil)))

  (defn focus--wrap-move-focus (fn)
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
             fn))))

  (setq focus-update-idle-time 0.2)
  (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . lisp))
  (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
  (add-to-list 'focus-mode-to-thing '(text-mode . sentence+))
  (put 'tex-sentence 'bounds-of-thing-at-point #'focus--tex-thing)
  (put 'sentence+    'bounds-of-thing-at-point #'focus--text-thing)
  (put 'lisp         'bounds-of-thing-at-point #'focus--lisp-thing)

  (advice-add #'focus-terminate :after #'focus--clear-update-timer)
  (advice-add #'focus-move-focus :around #'focus--wrap-move-focus))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook ((prog-mode rpm-spec-mode) . highlight-numbers-mode))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)

  (make-thread #'global-hl-line-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (defn hl-todo--setup-custom ()
    "TODO"
    (font-lock-remove-keywords nil hl-todo--keywords)
    (setq hl-todo--keywords
          (list (list (caar hl-todo--keywords)
                      `(1 (hl-todo--get-face) prepend))))
    (font-lock-add-keywords nil hl-todo--keywords t))

  (advice-add #'hl-todo--get-face :filter-return #'list)
  (advice-add #'hl-todo--setup :after #'hl-todo--setup-custom))

(use-package powerline
  :ensure t
  :config
  (when window-system
    (setq powerline-height (+ (frame-char-height) line-spacing)
          powerline-text-scale-factor 1)))

(use-package spaceline
  :ensure t
  :config
  (defn spaceline--wrap-unicode-number (fn str)
    "TODO"
    (let ((ret (funcall fn str)))
      (if (<= 11 (string-to-number str))
          ret
        (propertize ret 'display '(raise 0.05)))))

  (require 'spaceline-config)
  (setq spaceline-window-numbers-unicode t)

  (advice-add #'spaceline--unicode-number :around #'spaceline--wrap-unicode-number)

  (make-thread
   (lambda ()
     (let ((fmt (spaceline-emacs-theme)))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (setq mode-line-format fmt)))
       (spaceline-toggle-hud-off)
       (force-mode-line-update t)
       (spaceline-helm-mode)))))

(use-package vi-tilde-fringe
  :if (fboundp 'define-fringe-bitmap)
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'vi-tilde-fringe)
    (quelpa '(vi-tilde-fringe :fetcher github :repo "syl20bnr/vi-tilde-fringe")))
  (make-thread #'global-vi-tilde-fringe-mode))

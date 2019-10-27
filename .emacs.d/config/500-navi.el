(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-find-dir
             helm-projectile-find-file
             helm-projectile-switch-project))

(use-package neotree
  :ensure t
  :init
  (defn neo-buffer--back (path _)
    "TODO"
    (when (neo-buffer--expanded-node-p path)
      (neo-buffer--toggle-expand path)
      (neotree-refresh)))

  (defn neotree-back ()
    "TODO"
    (interactive)
    (unless (neo-buffer--expanded-node-p (neo-buffer--get-filename-current-line))
      (neotree-select-up-node))
    (neo-buffer--execute nil null-fn #'neo-buffer--back))

  (defn neotree-project-dir ()
    "TODO"
    (interactive)
    (let ((buf-file-name buffer-file-name))
      (neotree-dir (or (projectile-project-root)
                       (file-name-directory (or buf-file-name ""))
                       "~/"))
      (when (and neo-smart-open
                 buf-file-name)
        (neotree-find buf-file-name)
        (recenter))))

  (defn neotree-toggle-maximize ()
    "TODO"
    (interactive)
    (neo-window--zoom (if (< neo-window-width (window-width)) 'minimize 'maximize)))

  (defn neotree-create-dir (&optional dir)
    "TODO"
    (interactive "f")
    (neotree-create-node (concat dir "/")))

  :config
  (setq neo-auto-indent-point t
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-smart-open t
        neo-theme 'icons
        neo-window-width 35))

(use-package dumb-jump
  :ensure t
  :init
  (defvar helm-dumb-jump--keyword nil
    "TODO")

  (defvar helm-dumb-jump--proj nil
    "TODO")

  (defface helm-dumb-jump-match
    '((t (:inherit helm-match)))
    "TODO")

  (defn helm-dumb-jump--after-get-results (info)
    "TODO"
    (setq helm-dumb-jump--proj (plist-get info :root)
          helm-dumb-jump--keyword (plist-get info :symbol))
    info)

  (defn helm-dumb-jump--action (find-file-fn candidate)
    "TODO"
    (let* ((candidate (helm-grep-split-line candidate))
           (file (nth 0 candidate))
           (line (nth 1 candidate)))
      (if (fboundp 'xref-push-marker-stack)
          (xref-push-marker-stack)
        (ring-insert find-tag-marker-ring (point-marker)))
      (funcall find-file-fn (concat helm-dumb-jump--proj "/" file))
      (goto-char (point-min))
      (when line
        (forward-line (1- (string-to-number line)))
        (beginning-of-line)
        (when (re-search-forward (regexp-quote helm-dumb-jump--keyword)
                                 (line-end-position) 'noerr)
          (goto-char (match-beginning 0))))
      (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
        (run-hooks 'dumb-jump-after-jump-hook))))

  (defn helm-dumb-jump--insert-file (file)
    "TODO"
    (let ((def-dir default-directory))
      (switch-to-buffer (get-buffer-create "*helm-dumb-jump: persistent*"))
      (erase-buffer)
      (setq buffer-file-name file
            default-directory def-dir)
      (insert-file-contents file)
      (set-auto-mode)
      (font-lock-fontify-buffer)))

  (defn helm-dumb-jump--persistent-action (candidate)
    "TODO"
    (helm-dumb-jump--action #'helm-dumb-jump--insert-file candidate))

  (defn helm-dumb-jump--result-follow (result &optional use-tooltip proj)
    "TODO"
    (let* ((path (plist-get result :path))
           (path (if (file-name-absolute-p path)
                     path
                   (concat helm-dumb-jump--proj "/" (plist-get result :path))))
           (line (plist-get result :line))
           (pos (->> (plist-get result :context)
                     (s-split helm-dumb-jump--keyword)
                     (first)
                     (length))))
      (dumb-jump-goto-file-line path line pos)))

  (defn helm-dumb-jump--prompt-user-for-choice (proj results)
    "TODO"
    (when (eq 'helm dumb-jump-selector)
      (let* ((proj-regex (concat "^" (regexp-quote proj) "/*"))
             (paths (->> results
                         (--map (plist-get it :path))
                         (--map (s-replace-regexp proj-regex "" it))
                         (--map (propertize it 'face 'helm-moccur-buffer))))
             (lines (->> results
                         (--map (plist-get it :line))
                         (--map (number-to-string it))
                         (--map (propertize it 'face 'helm-grep-lineno))))
             (ctxs  (->> results
                         (--map (plist-get it :context))
                         (--map (s-trim it))
                         (--map (s-split (regexp-quote helm-dumb-jump--keyword) it))
                         (--map (-interpose (propertize helm-dumb-jump--keyword 'face 'helm-dumb-jump-match) it))
                         (--map (apply #'concat " " it))))
             (candidates (->> (-zip-pair paths lines ctxs)
                              (--map (-interpose ":" it))
                              (--map (apply #'concat it)))))
        (helm :sources
              (helm-build-sync-source "Dump Jump"
                :candidates candidates
                :action (helm-make-actions
                         "Open file"              (-partial #'helm-dumb-jump--action #'find-file)
                         "Open file other window" (-partial #'helm-dumb-jump--action #'find-file-other-window))
                :persistent-action #'helm-dumb-jump--persistent-action
                :after-init-hook
                (lambda ()
                  (with-current-buffer (get-buffer-create "*helm-dumb-jump*")
                    (setq-local helm-cur-line-highlight-symbols '(helm-dumb-jump--keyword)))))
              :buffer "*helm-dumb-jump*"))
      t))

  (defn dumb-jump--custom-get-language (file)
    "Get language from FILE extension and then fallback to using 'major-mode' name."
    (let* ((languages (-distinct
                       (--map (plist-get it :language)
                              dumb-jump-find-rules)))
           (language (or (dumb-jump-get-language-from-mode) ; I just changed order of this
                         (dumb-jump-get-language-by-filename file))))
      (if (member language languages)
          language
        (format ".%s file" (or (f-ext file) "")))))

  :config
  (setq dumb-jump-git-grep-cmd "git grep --full-name"
        dumb-jump-force-searcher (cond
                                   ((executable-find "rg") 'rg)
                                   ((executable-find "ag") 'ag)
                                   (t nil))
        dumb-jump-selector 'helm)

  (add-to-list 'dumb-jump-find-rules
               (list :type "function"
                     :supports '("ag" "grep" "rg" "git-grep")
                     :language "perl"
                     :regex "sub\\s*JJJ\\j"))

  (advice-add #'dumb-jump-get-results :filter-return #'helm-dumb-jump--after-get-results)
  (advice-add #'dumb-jump--result-follow :override #'helm-dumb-jump--result-follow)
  (advice-add #'dumb-jump-get-language :override #'dumb-jump--custom-get-language)
  (advice-add #'dumb-jump-prompt-user-for-choice :before-until
              #'helm-dumb-jump--prompt-user-for-choice))

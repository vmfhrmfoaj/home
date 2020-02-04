(use-package dumb-jump
  :ensure t
  :config
  (defvar helm-dumb-jump--keyword nil
    "TODO")

  (defvar helm-dumb-jump--proj nil
    "TODO")

  (defface helm-dumb-jump-keyword
    '((t (:inherit highlight)))
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
      (let ((proj-regex (concat "^" (regexp-quote proj) "/*"))
            candidates)
        (dolist (res results)
          (let ((path (s-replace-regexp proj-regex "" (plist-get res :path)))
                (line-num (number-to-string (plist-get res :line)))
                (line (plist-get res :context))
                (i 0)
                match-start-pos)
            (while (and (string-match (regexp-quote helm-dumb-jump--keyword) line i)
                        (not (= (match-beginning 0) (match-end 0))))
              (setq i (match-end 0)
                    line (concat (substring line 0 (match-beginning 0))
                                 (propertize (substring line (match-beginning 0) (match-end 0))
                                             'face 'helm-dumb-jump-keyword)
                                 (substring line (match-end 0)))))
            (add-to-list 'candidates
                         (concat (propertize path 'face 'helm-moccur-buffer) ":"
                                 (propertize line-num 'face 'helm-grep-lineno) ":"
                                 line)
                         t)))
        ;; (helm-set-local-variable
        ;;  'helm-cur-line-highlight-symbols '(helm-dumb-jump--keyword))
        (helm :sources
              (helm-build-sync-source "Dump Jump"
                :candidates candidates
                :action (helm-make-actions
                         "Open file"              (-partial #'helm-dumb-jump--action #'find-file)
                         "Open file other window" (-partial #'helm-dumb-jump--action #'find-file-other-window))
                :persistent-action #'helm-dumb-jump--persistent-action
                :match-part
                (byte-compile
                 (lambda (candidate)
                   (if (string-match "^.+?:[0-9]+:\\(.+\\)$" candidate)
                       (match-string 1 candidate)
                     candidate))))
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

  (defvar dumb-jump-search-current-directory-first nil
    "TODO")

  (defn dumb-jump--custom-fetch-file-results (&optional prompt)
    "TODO"
    (let* ((cur-file (or (buffer-file-name) ""))
           (cur-dir (file-name-directory cur-file))
           (proj-root (s-chop-suffix "/" (dumb-jump-get-project-root cur-file)))
           (proj-config (file-truename (or (dumb-jump-get-config proj-root) "")))
           (config (when (s-ends-with? ".dumbjump" proj-config)
                     (dumb-jump-read-config proj-root proj-config)))
           (lang (or (plist-get config :language)
                     (dumb-jump-get-language cur-file))))
      (catch 'found
        (when dumb-jump-search-current-directory-first
          (let ((res (dumb-jump-fetch-results cur-file cur-dir lang config prompt)))
            (when (plist-get res :results)
              (throw 'found (plist-put res :root proj-root))))
          (setq config (->> cur-dir
                            (-snoc (plist-get config :exclude))
                            (plist-put config :exclude))))
        (dumb-jump-fetch-results cur-file proj-root lang config prompt))))

  (defn dumb-jump--custom-fetch-results (cur-file proj-root lang config &optional prompt)
    "Fix for `config'."
    (let* ((cur-line (if prompt 0 (dumb-jump-get-point-line)))
           (look-for-start (when (not prompt)
                             (- (car (bounds-of-thing-at-point 'symbol))
                                (point-at-bol))))
           (cur-line-num (line-number-at-pos))
           (proj-config (dumb-jump-get-config proj-root))
           (config (or config ; <-- FIX HERE
                       (when (s-ends-with? ".dumbjump" proj-config)
                         (dumb-jump-read-config proj-root proj-config))))
           (found-symbol (or prompt (dumb-jump-get-point-symbol)))
           (look-for (or prompt (dumb-jump-process-symbol-by-lang lang found-symbol)))
           (pt-ctx (when (and (not prompt) (not (string= cur-line look-for)))
                     (dumb-jump-get-point-context cur-line look-for look-for-start)))
           (ctx-type (dumb-jump-get-ctx-type-by-language lang pt-ctx))
           (gen-funcs (dumb-jump-pick-grep-variant proj-root))
           (parse-fn (plist-get gen-funcs :parse))
           (generate-fn (plist-get gen-funcs :generate))
           (searcher (plist-get gen-funcs :searcher))
           (regexes (dumb-jump-get-contextual-regexes lang ctx-type searcher))
           (exclude-paths (when config (plist-get config :exclude)))
           (include-paths (when config (plist-get config :include)))
           ;; we will search proj root and all include paths
           (search-paths (-distinct (-concat (list proj-root) include-paths)))
           ;; run command for all
           (raw-results (--mapcat
                         ;; TODO: should only pass exclude paths to actual project root
                         (dumb-jump-run-command look-for it regexes lang exclude-paths cur-file
                                                cur-line-num parse-fn generate-fn)
                         search-paths))
           (results (delete-dups (--map (plist-put it :target look-for) raw-results))))
      (list :results results
            :lang (if (null lang) "" lang)
            :symbol look-for
            :ctx-type (if (null ctx-type) "" ctx-type)
            :file cur-file
            :root proj-root)))

  (setq dumb-jump-git-grep-cmd "git grep --full-name"
        dumb-jump-fallback-search nil
        dumb-jump-force-searcher (cond
                                   ((executable-find "rg") 'rg)
                                   ((executable-find "ag") 'ag)
                                   (t nil))
        dumb-jump-max-find-time 20 ; for large project such as linux kernel
        dumb-jump-search-current-directory-first t
        dumb-jump-selector 'helm)

  (with-eval-after-load "cc-mode"
    (-update->> dumb-jump-find-rules
                (--remove (string= "c++" (plist-get it :language)))
                (-concat (list
                          (list :type "function"
                                :supports '("ag" "rg")
                                :language "c++"
                                :regex "(^\\s*(static\\s+)?((struct|union)\\s+)?[_:0-9A-Za-z]+(\\s*\\*)?\\s+JJJ\\s*\\\([^\\)]*(\\\)(\\s*\\{)?|,)\\s*$)")
                          (list :type "function"
                                :supports '("ag" "grep" "rg" "git-grep")
                                :language "c++"
                                :regex "(^\\s*#define\\s+JJJ\\\()")
                          ;; original
                          (list :type "type"
                                :supports '("ag" "rg" "git-grep")
                                :language "c++"
                                :regex "\\b(class|struct|enum|union)\\b\\s*JJJ\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*JJJ\\b\\s*;")
                          (list :type "variable"
                                :supports '("ag" "rg")
                                :language "c++"
                                :regex "(^\\s*(static\\s+)?((struct|union)\\s+)?[_:0-9A-Za-z]+(\\s*\\*)?\\s+JJJ\\s*(;\\s*$|=))|#define\\s+JJJ\\b")))))
  (with-eval-after-load "cperl-mode"
    (add-to-list 'dumb-jump-find-rules
                 (list :type "function"
                       :supports '("ag" "grep" "rg" "git-grep")
                       :language "perl"
                       :regex "sub\\s*JJJ\\j")))

  (advice-add #'dumb-jump--result-follow :override #'helm-dumb-jump--result-follow)
  (advice-add #'dumb-jump-fetch-file-results :override #'dumb-jump--custom-fetch-file-results)
  (advice-add #'dumb-jump-fetch-results :override #'dumb-jump--custom-fetch-results)
  (advice-add #'dumb-jump-get-results :filter-return #'helm-dumb-jump--after-get-results)
  (advice-add #'dumb-jump-get-language :override #'dumb-jump--custom-get-language)
  (advice-add #'dumb-jump-prompt-user-for-choice :before-until #'helm-dumb-jump--prompt-user-for-choice))

(use-package helm-xref
  :ensure t
  :defer t
  :commands (helm-xref-show-defs-27 helm-xref-show-xrefs-27)
  :init
  (defalias 'xref-pop-to-location #'xref--pop-to-location)
  (setq xref-show-definitions-function 'helm-xref-show-defs-27
        xref-show-xrefs-function 'helm-xref-show-xrefs-27))

(use-package neotree
  :ensure t
  :config
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

  (setq neo-auto-indent-point t
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-smart-open t
        neo-theme 'icons
        neo-window-width 35)

  (add-hook 'neotree-mode-hook (lambda () (setq-local right-fringe-width 0))))

(use-package xref
  :after (persp-mode projectile)
  :config
  (defn xref--change-persp-after-pop-marker ()
    "TODO"
    (let* ((persp-root (-some->> (persp-current-project)
                         (file-truename)))
           (proj-root (-some->> buffer-file-name
                        (file-name-directory)
                        (projectile-project-root)
                        (file-truename))))
      (when (and persp-root
                 proj-root
                 (not (s-starts-with? persp-root proj-root)))
        (let ((cur-buf (current-buffer))
              (proj (abbreviate-file-name proj-root)))
          (switch-to-previous-buffer)
          (persp-switch proj)
          (switch-to-buffer cur-buf)))))

  ;; See, `lsp--change-proj' function
  (advice-add #'xref-pop-marker-stack :after
              #'xref--change-persp-after-pop-marker))

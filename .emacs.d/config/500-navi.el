;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package dumb-jump
  :ensure t
  :config
  (defun dumb-jump--custom-get-language (file)
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

  (defun dumb-jump--custom-fetch-file-results (&optional prompt)
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

  (defun dumb-jump--custom-fetch-results (cur-file proj-root lang config &optional prompt)
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
        dumb-jump-selector 'ivy)

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

  (advice-add #'dumb-jump-fetch-file-results :override #'dumb-jump--custom-fetch-file-results)
  (advice-add #'dumb-jump-fetch-results :override #'dumb-jump--custom-fetch-results)
  (advice-add #'dumb-jump-get-language :override #'dumb-jump--custom-get-language))

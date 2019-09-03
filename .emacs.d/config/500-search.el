(use-package helm-swoop
  :ensure t
  :defer t
  :init
  (defn helm-swoop--colorize (it)
    (propertize it 'font-lock-face 'helm-swoop-line-number-face))

  (defn helm-swoop--get-content-for-fancy-narrow (buf &optional linum)
    "TODO"
    (let* (ret
           (buf (or (get-buffer buf) (current-buffer)))
           ;; NOTE:
           ;;  a advice of the compiled fundamental functions is not working.
           ;;  see http://nullprogram.com/blog/2013/01/22/
           (pos-min 0)
           (pos-max 0)
           (str (with-current-buffer buf
                  (setq pos-min (or fancy-narrow--beginning (point-min))
                        pos-max (or fancy-narrow--end (point-max)))
                  (buffer-substring-no-properties pos-min pos-max)))
           (num (line-number-at-pos pos-min))
           (fmt (concat "%-"
                        (-> pos-max
                            (line-number-at-pos)
                            (number-to-string)
                            (length)
                            (number-to-string))
                        "s "))
           (insert-linum (-compose #'insert
                                   (if helm-swoop-use-line-number-face
                                       #'helm-swoop--colorize
                                     #'identity)
                                   (-partial #'format fmt))))
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (funcall insert-linum num)
        (while (progn (forward-line) (not (eobp)))
          (-update-> num (1+))
          (if (and (not linum)
                   (looking-at-p "^[0-9]+\\s-*$"))
              (kill-line)
            (funcall insert-linum num)))
        (setq ret (buffer-substring (point-min) (point-max))))
      ret))

  (defn helm-swoop--clear-cache-hard (&rest _)
    "TODO"
    (kill-local-variable 'helm-swoop-list-cache))

  (defn helm-swoop--refine-candidate (str &optional pattern)
    (list (-last-item (s-split-up-to "[0-9]+" str 1))
          (or pattern helm-pattern)))

  (defn helm-swoop--modify-action-on-helm-source (source)
    (--map (if (eq 'action (car it))
               `(action . ,(append (cdr it) '(("Edit" . helm-swoop--edit))))
             it)
           source))

  :config
  (setq helm-swoop-pre-input-function (-const "")
        helm-swoop-speed-or-color nil
        helm-swoop-split-window-function #'helm-display-buffer-at-bottom
        helm-swoop-use-line-number-face t
        helm-c-source-swoop-match-functions
        (->> '(helm-mm-exact-match
               helm-mm-match
               helm-mm-3-migemo-match)
             (--map (-partial #'apply it))
             (--map (-compose it #'helm-swoop--refine-candidate)))
        helm-c-source-swoop-search-functions
        (->> '(helm-mm-exact-search
               helm-mm-search
               helm-candidates-in-buffer-search-default-fn
               helm-mm-3-migemo-search)
             (--map (-partial #'apply it))
             (--map (-compose it #'helm-swoop--refine-candidate))))
  (advice-add #'helm-swoop :after (byte-compile (lambda (&rest _) (keyboard-quit)))) ; NOTE: How to cancel the selection?
  (advice-add #'helm-swoop--get-content :override #'helm-swoop--get-content-for-fancy-narrow)
  (advice-add #'helm-c-source-swoop       :filter-return #'helm-swoop--modify-action-on-helm-source)
  (advice-add #'helm-c-source-multi-swoop :filter-return #'helm-swoop--modify-action-on-helm-source))

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (defn helm-ag--custom-do-ag-candidate-process ()
    (let* ((non-essential nil)
           (default-directory (or helm-ag--default-directory
                                  helm-ag--last-default-directory
                                  default-directory))
           (cmd-args (helm-ag--construct-do-ag-command helm-pattern)))
      (when cmd-args
        (let ((proc (apply #'start-file-process "helm-do-ag" nil cmd-args)))
          (setq helm-ag--last-query helm-pattern
                helm-ag--last-command cmd-args
                helm-ag--ignore-case (helm-ag--ignore-case-p cmd-args helm-pattern)
                helm-ag--last-default-directory default-directory)
          (prog1 proc
            (set-process-sentinel
             proc
             (lambda (process event)
               (helm-process-deferred-sentinel-hook
                process event (helm-default-directory))
               (when (string-match-p "^\\(finished\\|exited\\|failed\\)" event)
                 (ignore-errors
                   (helm-ag--do-ag-propertize helm-input))))))))))

  (defn helm-do-ag-wrap (fn &optional basedir targets)
    "Wrap `helm-do-ag' to change `helm-input-idle-delay' in the `helm-do-ag' context."
    (let ((helm-input-idle-delay 0.2))
      (funcall fn basedir targets)))

  (defn helm-ag--elisp-regexp-to-pcre-for-ripgrep (pattern)
    "Improve `helm-ag--elisp-regexp-to-pcre' for ripgrep."
    (s-replace-all '(("\\s-" . "[[:space:]]")) pattern))

  (defn helm-ag--find-file-action-for-vlf (candidate find-func this-file &optional persistent)
    "antoehr version of `helm-ag--find-file-action' for `vlf-mode'."
    (when vlf-mode
      (when (memq 'pt helm-ag--command-features)
        ;; 'pt' always show filename if matched file is only one.
        (setq this-file nil))
      (let* ((file-line (helm-grep-split-line candidate))
             (filename (or this-file (cl-first file-line) candidate))
             (line (if this-file
                       (cl-first (split-string candidate ":"))
                     (cl-second file-line)))
             (default-directory (or helm-ag--default-directory
                                    helm-ag--last-default-directory
                                    default-directory)))
        (unless persistent
          (setq helm-ag--last-default-directory default-directory))
        (funcall find-func filename)
        (vlf-goto-line (string-to-number line))
        (beginning-of-line)
        (ignore-errors
          (and (re-search-forward helm-ag--last-query (line-end-position) t)
               (goto-char (match-beginning 0))))
        t)))

  :config
  (setq helm-ag-base-command "rg"
        helm-ag-command-option "--no-heading --no-messages --smart-case"
        helm-ag-use-emacs-lisp-regexp t)
  (advice-add #'helm-ag--do-ag-candidate-process :override
              #'helm-ag--custom-do-ag-candidate-process)
  (advice-add #'helm-do-ag :around #'helm-do-ag-wrap)
  (advice-add #'helm-ag--elisp-regexp-to-pcre :filter-return #'helm-ag--elisp-regexp-to-pcre-for-ripgrep)
  (with-eval-after-load "projectile"
    (advice-add #'helm-ag--project-root :override #'projectile-project-root))
  (with-eval-after-load "vlf"
    (advice-add #'helm-ag--find-file-action :before-until #'helm-ag--find-file-action-for-vlf)))

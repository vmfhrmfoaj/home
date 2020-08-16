;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/config/func.el"))

(use-package helm-ag
  :ensure t
  :defer t
  :init
  (defun helm-do-ag-with-no-ignore ()
    "TODO"
    (interactive)
    (let ((helm-ag-command-option (concat "--no-ignore " helm-ag-command-option)))
      (call-interactively #'helm-do-ag)))

  (defun helm-do-ag-project-root-with-no-ignore ()
    "TODO"
    (interactive)
    (let ((helm-ag-command-option (concat "--no-ignore " helm-ag-command-option)))
      (call-interactively #'helm-do-ag-project-root)))

  (setq helm-ag-base-command "rg"
        helm-ag-command-option "--no-heading --no-messages --smart-case"
        helm-ag-use-emacs-lisp-regexp t)

  :config
  (defun helm-ag--custom-do-ag-candidate-process (dir)
    "TODO"
    (let* ((non-essential nil)
           (default-directory dir)
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

  (defun helm-ag--elisp-regexp-to-pcre-for-ripgrep (pattern)
    "Improve `helm-ag--elisp-regexp-to-pcre' for ripgrep."
    (s-replace-all '(("\\s-" . "[[:space:]]")) pattern))

  (defun helm-ag--find-file-action-for-vlf (candidate find-func this-file &optional persistent)
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

  (defun helm-ag--custom-propertize-candidates (input)
    "TODO"
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (let ((patterns (helm-ag--do-ag-highlight-patterns input)))
        (cl-loop with one-file-p = (and (not (helm-ag--vimgrep-option))
                                        (helm-ag--search-only-one-file-p))
                 while (not (eobp))
                 for num = 1 then (1+ num)
                 do
                 (progn
                   (let ((start (point))
                         (bound (line-end-position)))
                     (if (and one-file-p (search-forward ":" bound t))
                         (set-text-properties (line-beginning-position) (1- (point))
                                              '(face helm-grep-lineno))
                       (when (re-search-forward helm-grep-split-line-regexp bound t)
                         (set-text-properties (match-beginning 1) (match-end 1) '(face helm-moccur-buffer))
                         (set-text-properties (match-beginning 2) (match-end 2) '(face helm-grep-lineno))
                         (goto-char (match-beginning 3))))
                     (let ((curpoint (point))
                           (case-fold-search helm-ag--ignore-case))
                       (dolist (pattern patterns)
                         (let ((idx 0)
                               (last-point (point)))
                           (when (s-starts-with? "^" pattern)
                             (beginning-of-line)
                             (setq idx 1
                                   pattern (concat "^\\(?:[[:lower:][:upper:]]?:?.*?\\):\\(?:[0-9]+\\):\\("
                                                   (substring pattern 1)
                                                   "\\)")))
                           (while (re-search-forward pattern bound t)
                             (set-text-properties (match-beginning idx) (match-end idx)
                                                  '(face helm-match))
                             (when (= last-point (point))
                               (forward-char 1))
                             (setq last-point (point)))
                           (goto-char curpoint))))
                     (put-text-property start bound 'helm-cand-num num))
                   (forward-line 1))))))

  (defun helm-ag--set-input-idle-delay (&rest _)
    "Locally set `helm-input-idle-delay' to `helm-grep-input-idle-delay'."
    (helm-set-local-variable
     'helm-input-idle-delay helm-grep-input-idle-delay))

  (defun helm-ag--custom-project-root ()
    "TODO"
    (and (featurep 'projectile)
         (projectile-project-root)))

  (advice-add #'helm-ag--do-ag-candidate-process :override #'helm-ag--custom-do-ag-candidate-process)
  (advice-add #'helm-ag--propertize-candidates :override #'helm-ag--custom-propertize-candidates)
  (advice-add #'helm-do-ag--helm :before #'helm-ag--set-input-idle-delay)
  (advice-add #'helm-ag--elisp-regexp-to-pcre :filter-return #'helm-ag--elisp-regexp-to-pcre-for-ripgrep)
  (advice-add #'helm-ag--project-root :before-until #'helm-ag--custom-project-root)
  (with-eval-after-load "vlf"
    (advice-add #'helm-ag--find-file-action :before-until #'helm-ag--find-file-action-for-vlf)))

(use-package helm-grep
  :defer t
  :config
  (setq helm-grep-input-idle-delay 0.2))

(use-package helm-occur
  :defer t
  :config
  (defun helm-occur--switch-to-helm-window-after-action (&rest _)
    "Return the focus to `helm-window' to avoid flickering."
    (global-hl-line-highlight)
    (-when-let (win (helm-window))
      (select-window win)))

  (defun helm-occur--edit (candidate)
    "TODO"
    (let ((buf-name "*hmoccur*"))
      (ignore-errors (kill-buffer buf-name))
      (helm-occur-save-results candidate)
      (-when-let (buf (get-buffer buf-name))
        (with-current-buffer buf
          (wgrep-change-to-wgrep-mode)
          (local-set-key (kbd "C-c C-c") (lambda () (interactive) (wgrep-finish-edit) (kill-buffer-and-window)))
          (local-set-key (kbd "C-c C-k") (lambda () (interactive)                     (kill-buffer-and-window)))))))

  (defun helm-occur-edit ()
    "TODO"
    (interactive)
    (helm-exit-and-execute-action 'helm-occur--edit))

  (-update-> helm-occur-actions (append '(("Edit search results" . helm-occur--edit))))

  (advice-add #'helm-occur-action :after #'helm-occur--switch-to-helm-window-after-action))

(use-package wgrep-helm
  :ensure t
  :defer t)

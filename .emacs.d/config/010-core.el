;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package evil
  :ensure t
  :config
  (evil-define-text-object evil-inner-sexp (count &optional beg end type)
    "Select a sp-sexp."
    :extend-selection nil
    (let ((beg (save-excursion
                 (sp-backward-up-sexp)
                 (sp-down-sexp)
                 (point)))
          (end (save-excursion
                 (sp-end-of-sexp)
                 (1- (point)))))
      (list beg end type)))

  (evil-define-text-object evil-a-sexp (count &optional beg end type)
    "Select a sp-sexp."
    (let ((beg (save-excursion
                 (sp-backward-up-sexp)
                 (point)))
          (end (save-excursion
                 (sp-up-sexp)
                 (point))))
      (list beg end type :expanded t)))

  (setq-default evil-echo-state nil
                evil-symbol-word-search t
                evil-want-minibuffer t)

  (evil-mode))

(use-package evil-ex
  :ensure evil
  :config
  (defvar-local evil-ex--gl-preview-point nil
    "TODO")

  (defun evil-ex-update-for--goto-line-preview (&optional beg end len string)
    "TODO"
    ;; (print (list 'env (selected-window) (current-buffer) evil-ex-current-buffer '|
    ;;              'parameters beg end len string '|
    ;;              'variables evil-ex-tree evil-ex-expression evil-ex-range evil-ex-cmd evil-ex-bang evil-ex-argument))
    (when (eq 'evil-goto-line (car evil-ex-expression))
      (-when-let (win (-some->> (window-list)
                                (--filter (eq evil-ex-current-buffer (window-buffer it)))
                                (-first-item)))
        (with-selected-window win
          (unless evil-ex--gl-preview-point
            (setq-local evil-ex--gl-preview-point (point)))
          (let ((line-num (eval (cadadr evil-ex-expression))))
            (goto-line line-num evil-ex-current-buffer)
            (redisplay t)
            (when (bound-and-true-p linum-mode)
              (linum-update evil-ex-current-buffer))
            (when (and (bound-and-true-p hl-line-mode) hl-line-overlay)
              (hl-line-highlight))
            (when (and (bound-and-true-p global-hl-line-mode) global-hl-line-mode)
              (global-hl-line-highlight)))))))

  (defun abort-recursive-edit-for-evil-ex ()
    "TODO"
    (interactive)
    (-when-let (win (-some->> (window-list)
                              (--filter (eq evil-ex-current-buffer (window-buffer it)))
                              (-first-item)))
      (with-selected-window win
        (when evil-ex--gl-preview-point
          (goto-char evil-ex--gl-preview-point))))
    (abort-recursive-edit))

  (advice-add #'evil-ex-setup :before
              (lambda ()
                "setup for `evil-ex-update-for--goto-line-preview' function."
                (with-current-buffer evil-ex-current-buffer
                  (setq-local evil-ex--gl-preview-point nil))))
  (advice-add #'evil-ex-update :after #'evil-ex-update-for--goto-line-preview)
  (advice-add #'evil-ex-execute :before
              (lambda (_)
                "restore the position of the cursor for `evil-ex-update-for--goto-line-preview' function."
                (when evil-ex--gl-preview-point
                  (goto-char evil-ex--gl-preview-point))))

  (setq evil-ex-visual-char-range t))

(use-package helm
  :ensure t
  :config
  (defvar helm-search-buffer-regex (regexp-opt '("*helm-ag*" "*helm occur*" "*helm-xref*"))
    "TODO")

  (defvar helm-last-search-buffer nil
    "TODO")

  (defun helm--update-last-search-buffer (&rest _)
    "TODO"
    (interactive)
    (when (and (stringp helm-last-buffer)
               (string-match-p helm-search-buffer-regex helm-last-buffer))
      (setq helm-last-search-buffer helm-last-buffer)))

  (defun helm-resume-last-search-buffer ()
    "TODO"
    (interactive)
    ;; NOTE
    ;; (helm-resume helm-last-search-buffer)
    ;;  for `helm-swoop-resume' advice function:
    (if (not (and (stringp helm-last-search-buffer) (get-buffer helm-last-search-buffer)))
        (message "There is no search result!")
      (setq helm-last-buffer helm-last-search-buffer)
      (call-interactively #'helm-resume)))

  (defun helm-display-buffer-at-bottom (buffer &optional resume)
    "TODO"
    (let ((display-buffer-alist '(("\\*.*[Hh]elm.*\\*"
                                   (display-buffer-in-side-window)
                                   (inhibit-same-window . t)
                                   (side . bottom)))))
      (helm-default-display-buffer buffer resume)))

  (defface helm-match-selection
    `((t (:inherit helm-match)))
    "TODO"
    :group 'helm-faces)

  (defface helm-other-buffer
    `((t (:inherit shadow)))
    "TODO"
    :group 'helm-faces)

  (defvar helm-match-selection-overlays nil
    "TODO")

  (defvar helm-cur-line-highlight-symbols '()
    "TODO")

  (make-local-variable 'helm-match-selection-overlays)

  (defun helm-custom-initialize-overlays (_buffer)
    "TODO"
    (dolist (ov helm-match-selection-overlays)
      (delete-overlay ov))
    (setq helm-match-selection-overlays nil))

  (defun helm--remove-custom-overlays ()
    "TODO"
    (dolist (ov helm-match-selection-overlays)
      (delete-overlay ov))
    (setq helm-match-selection-overlays nil))

  (defun helm--pattern-to-regex-list (pattern)
    "TODO"
    (unless (s-blank-str? pattern)
      (->> pattern
           (s-replace "\\ " "\\s-")
           (s-split " ")
           (--mapcat (s-split "\\s-+" it))
           (-remove #'s-blank-str?))))

  (defun helm-custom-mark-current-line (&optional _resumep _nomouse)
    "TODO"
    (helm--remove-custom-overlays)
    (let ((pattern (if (let ((case-fold-search t))
                         (-some->> (helm-get-current-source)
                                   (assoc 'name)
                                   (cdr)
                                   (string-match-p (regexp-opt '("file" "project")))))
                       (file-name-nondirectory helm-pattern)
                     helm-pattern)))
      (let ((regexes (->> pattern
                          (helm--pattern-to-regex-list)
                          (-list)
                          (--mapcat (s-split "\\s-+" it)))))
        (dolist (sym helm-cur-line-highlight-symbols regexes)
          (let ((val (ignore-errors (symbol-value sym))))
            (unless (s-blank-str? val)
              (setq regexes (-concat regexes (helm--pattern-to-regex-list val))))))
        (dolist (regex regexes)
          (save-excursion
            (beginning-of-line)
            (when (or (string= helm-buffer "*helm-dumb-jump*")
                      (string= helm-buffer "*helm-ag*")
                      (string= helm-buffer "*helm-grep*"))
             (re-search-forward "^\\(?:[[:lower:][:upper:]]?:?.*?[0-9]+:\\)?" (line-end-position) t))
            (-when-let (match-part (get-text-property (point) 'match-part))
              (and (re-search-forward (regexp-quote match-part) (line-end-position) t)
                   (goto-char (match-beginning 0))))
            (let* ((i 0)
                   (start-pos (point))
                   (end-pos (or (cond
                                 ((string= helm-buffer "*helm buffers*")
                                  (text-property-any (line-beginning-position) (line-end-position) 'face 'helm-buffer-size)))
                                (line-end-position)))
                   (line (buffer-substring start-pos end-pos)))
              (condition-case nil
                  (while (and (string-match regex line i)
                              (not (= (match-beginning 0) (match-end 0))))
                    (setq i (match-end 0))
                    (let ((ov (make-overlay (+ start-pos (match-beginning 0)) (+ start-pos (match-end 0)))))
                      (add-to-list 'helm-match-selection-overlays ov)
                      (overlay-put ov 'face 'helm-match-selection)
                      (overlay-put ov 'priority 2)))
                (error (helm--remove-custom-overlays)))))))))

  (defun helm--custom-symbol-name (obj)
    (if (symbolp obj)
        (symbol-name obj)
      "Anonymous"))

  (defvar-local helm-face-remap-cookie nil)

  (require 'helm-config)
  (setq helm-autoresize-min-height 25
        helm-autoresize-max-height 45
        helm-buffer-max-length 35
        helm-buffer-list-reorder-fn (lambda (visibles others)
                                      (nconc (sort-buffer-by-visit-time others) visibles))

        helm-display-header-line nil
        helm-ff-keep-cached-candidates nil
        helm-split-window-inside-p t
        helm-truncate-lines t
        helm-window-prefer-horizontal-split 'decide)

  (add-hook 'helm-before-initialize-hook (lambda () (setq gc-cons-threshold (* 4 gc-cons-threshold))))

  (defvar helm-dim-buffers nil)

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (when helm-alive-p
                (let ((helm-window (helm-window)))
                  (dolist (win (window-list))
                    (when-let ((buf (and (not (eq win helm-window))
                                         (window-buffer win))))
                      (with-current-buffer buf
                        (ignore-errors (setq helm-face-remap-cookie (face-remap-add-relative 'default 'helm-other-buffer))))
                      (add-to-list 'helm-dim-buffers buf)))))))
  (add-hook 'helm-cleanup-hook
            (lambda ()
              (dolist (buf helm-dim-buffers)
                (with-current-buffer buf
                  (when helm-face-remap-cookie
                    (ignore-errors (face-remap-remove-relative helm-face-remap-cookie))
                    (setq helm-face-remap-cookie nil))))
              (setq helm-dim-buffers nil)
              (setq gc-cons-threshold (get 'gc-cons-threshold 'default-value))
              (garbage-collect)))

  (advice-add #'helm-initialize-overlays :after #'helm-custom-initialize-overlays)
  (advice-add #'helm-mark-current-line   :after #'helm-custom-mark-current-line)
  (advice-add #'helm-initialize :after #'helm--update-last-search-buffer)
  (advice-add #'helm-symbol-name :override #'helm--custom-symbol-name)

  (make-thread (lambda ()
                 (helm-mode 1)
                 (helm-autoresize-mode 1)
                 (when (functionp 'diminish)
                   (diminish 'helm-mode)))))

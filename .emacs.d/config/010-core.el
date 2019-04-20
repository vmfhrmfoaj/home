(use-package evil
  :ensure t
  :init
  (defvar-local evil-ex--gl-preview-point nil
    "TODO")

  (defn evil-ex-update-for--goto-line-preview (&optional beg end len string)
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

  (defn abort-recursive-edit-for-evil-ex ()
    "TODO"
    (interactive)
    (-when-let (win (-some->> (window-list)
                              (--filter (eq evil-ex-current-buffer (window-buffer it)))
                              (-first-item)))
      (with-selected-window win
        (when evil-ex--gl-preview-point
          (goto-char evil-ex--gl-preview-point))))
    (abort-recursive-edit))

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

  (setq-default evil-symbol-word-search t)
  (define-key evil-ex-completion-map [remap abort-recursive-edit] #'abort-recursive-edit-for-evil-ex)
  (advice-add #'evil-ex-setup :before
              (byte-compile
               (lambda ()
                 "setup for `evil-ex-update-for--goto-line-preview' function."
                 (with-current-buffer evil-ex-current-buffer
                   (setq-local evil-ex--gl-preview-point nil)))))
  (advice-add #'evil-ex-update :after #'evil-ex-update-for--goto-line-preview)
  (advice-add #'evil-ex-execute :before
              (byte-compile
               (lambda (_)
                 "restore the position of the cursor for `evil-ex-update-for--goto-line-preview' function."
                 (when evil-ex--gl-preview-point
                   (goto-char evil-ex--gl-preview-point)))))
  (make-thread #'evil-mode))

(use-package evil-ex
  :ensure evil
  :config
  (setq evil-ex-visual-char-range t))

(use-package helm
  :ensure t
  :diminish ""
  :init
  (defn helm-bufferp (buf)
    "TODO"
    (when (and (bufferp buf)
               (string-match-p "\\*.*[Hh]elm.*\\*" (buffer-name buf)))
      t))

  (defvar helm-search-buffer-regex "\\*\\(?:helm-ag\\|Helm Swoop\\)\\*"
    "TODO")

  (defvar helm-last-search-buffer nil
    "TODO")

  (defn helm--update-last-search-buffer (&rest _)
    "TODO"
    (interactive)
    (when (and (stringp helm-last-buffer)
               (string-match-p helm-search-buffer-regex helm-last-buffer))
      (setq helm-last-search-buffer helm-last-buffer)))

  (defn helm-resume-last-search-buffer ()
    "TODO"
    (interactive)
    ;; NOTE
    ;; (helm-resume helm-last-search-buffer)
    ;;  for `helm-swoop-resume' advice function:
    (when (get-buffer helm-last-search-buffer)
      (setq helm-last-buffer helm-last-search-buffer)
      (call-interactively #'helm-resume)))

  (defn helm-display-buffer-at-bottom (buffer &optional resume)
    "TODO"
    (let ((display-buffer-alist '(("\\*.*[Hh]elm.*\\*"
                                   (display-buffer-in-side-window)
                                   (inhibit-same-window . t)
                                   (side . bottom)))))
      (helm-default-display-buffer buffer resume)))

  (defn helm-persistent-action-display-window-for-neotree (&optional _)
    "TODO"
    (with-helm-window
      (when (string-match-p "\\*.*NeoTree" (buffer-name helm-current-buffer))
        (get-buffer-window helm-current-buffer))))

  (defface helm-match-selection
    `((t (:inherit helm-match)))
    "TODO"
    :group 'helm-faces)

  (defvar helm-match-selection-overlays nil
    "TODO")

  (make-local-variable 'helm-match-selection-overlays)

  (defn helm-custom-initialize-overlays (_buffer)
    "TODO"
    (dolist (ov helm-match-selection-overlays)
      (delete-overlay ov))
    (setq helm-match-selection-overlays nil))

  (defn helm--remove-custom-overlays ()
    "TODO"
    (dolist (ov helm-match-selection-overlays)
      (delete-overlay ov))
    (setq helm-match-selection-overlays nil))

  (defn helm-custom-mark-current-line (&optional _resumep _nomouse)
    "TODO"
    (helm--remove-custom-overlays)
    (let ((pattern (if (let ((case-fold-search t))
                         (->> (helm-get-current-source)
                              (assoc 'name)
                              (cdr)
                              (string-match-p (regexp-opt '("file" "project")))))
                       (file-name-nondirectory helm-pattern)
                     helm-pattern)))
      (when (not (s-blank-str? pattern))
        (save-excursion
          (beginning-of-line)
          (while (-intersection '(helm-swoop-line-number-face helm-moccur-buffer helm-grep-file helm-grep-lineno)
                                (-concat (-list (get-text-property (point) 'face))
                                         (-list (get-text-property (point) 'font-lock-face))))
            (forward-char 1))
          (let ((regex (->> pattern
                            (s-replace "\\ " "\\s-")
                            (s-split " ")
                            (-remove #'s-blank-str?)
                            (--sort (< (length other) (length it)))
                            (-interpose "\\|")
                            (apply #'concat))))
            (condition-case nil
                (while (and (re-search-forward regex (line-end-position) t)
                            (not (= (match-beginning 0) (match-end 0))))
                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                    (add-to-list 'helm-match-selection-overlays ov)
                    (overlay-put ov 'face 'helm-match-selection)
                    (overlay-put ov 'priority 2)))
              (error (helm--remove-custom-overlays))))))))

  :config
  (require 'helm-config)
  (setq helm-autoresize-min-height 25
        helm-autoresize-max-height 45
        helm-display-header-line nil
        helm-display-function #'helm-display-buffer-at-bottom
        helm-split-window-inside-p t
        helm-truncate-lines t)
  (add-hook 'helm-before-initialize-hook (byte-compile (lambda () (setq gc-cons-threshold (* 1024 1024 128)))))
  (add-hook 'helm-cleanup-hook
            (byte-compile
             (lambda ()
               (setq gc-cons-threshold (* 1024 1024 32))
               (garbage-collect))))
  (advice-add #'helm-persistent-action-display-window :before-until
              #'helm-persistent-action-display-window-for-neotree)
  (advice-add #'helm-initialize-overlays :after #'helm-custom-initialize-overlays)
  (advice-add #'helm-mark-current-line   :after #'helm-custom-mark-current-line)
  (advice-add #'helm-initialize :after #'helm--update-last-search-buffer)
  (make-thread (lambda ()
                 (helm-mode 1)
                 (helm-autoresize-mode 1))))

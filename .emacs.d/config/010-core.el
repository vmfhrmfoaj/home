;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package counsel
  :ensure t
  :defer t
  :config
  (setf (alist-get 'counsel-yank-pop       ivy-height-alist) 15)
  (setf (alist-get 'counsel-evil-registers ivy-height-alist) 15)

  (defun counsel--custom-switch-buffer-update-fn ()
    (unless counsel--switch-buffer-previous-buffers
      (setq counsel--switch-buffer-previous-buffers (buffer-list)))
    ;; NOTE
    ;;  `counsel-switch-buffer' do not honor `ivy-call'.
    (when ivy-calling
      (let* ((virtual (assoc (ivy-state-current ivy-last) ivy--virtual-buffers)))
        (when (member (ivy-state-current ivy-last) ivy-marked-candidates)
          (setf (ivy-state-current ivy-last)
                (substring (ivy-state-current ivy-last) (length ivy-mark-prefix))))
        (cond
         ((get-buffer (ivy-state-current ivy-last))
          (let ((ivy-marked-candidates nil))
            (ivy-call)))
         ((and counsel-switch-buffer-preview-virtual-buffers virtual (file-exists-p (cdr virtual)))
          (let ((buf (ignore-errors
                       ;; may not open due to `large-file-warning-threshold' etc.
                       (find-file-noselect (cdr virtual)))))
            (if buf
                (progn
                  (push buf counsel--switch-buffer-temporary-buffers)
                  (ivy-call))
              ;; clean up the minibuffer so that there's no delay before
              ;; the Ivy candidates are displayed once again
              (message ""))))
         (t
          (with-ivy-window
            (switch-to-buffer (ivy-state-buffer ivy-last))))))))

  (advice-add #'counsel--switch-buffer-update-fn :override #'counsel--custom-switch-buffer-update-fn))

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
                evil-move-beyond-eol t
                evil-symbol-word-search t
                evil-want-minibuffer t)
  (setq evil-flash-delay 1)

  (remove-hook 'evil-goto-definition-functions 'evil-goto-definition-xref)

  (advice-add #'isearch-highlight :around
              (lambda (fn &rest args)
                "wrap with `ignore-errors'."
                (ignore-errors (apply fn args))))

  (evil-mode))

(use-package evil-search
  :defer t
  :config
  (defun evil--cusotm-flash-search-pattern (string &optional all)
    "Customize for lazy-highlight"
    (let ((lazy-highlight-initial-delay 0)
          (isearch-search-fun-function 'evil-isearch-function)
          (isearch-case-fold-search case-fold-search)
          (disable #'(lambda (&optional _arg) (evil-flash-hook t))))
      (when evil-flash-timer
        (cancel-timer evil-flash-timer))
      (unless (or (null string)
                  (string= string ""))
        (evil-echo-area-save)
        (evil-echo "%s" string)
        (isearch-highlight (match-beginning 0) (match-end 0))
        (when all
          (setq isearch-lazy-highlight-wrapped nil
                isearch-lazy-highlight-start (point)
                isearch-lazy-highlight-end (point))
          (isearch-lazy-highlight-new-loop))
        (add-hook 'pre-command-hook #'evil-flash-hook nil t)
        (add-hook 'evil-operator-state-exit-hook #'evil-flash-hook nil t)
        (add-hook 'pre-command-hook #'evil-clean-isearch-overlays nil t)
        (setq evil-flash-timer
              (run-at-time evil-flash-delay nil disable)))))

  (advice-add #'evil-flash-search-pattern :override #'evil--cusotm-flash-search-pattern))

(use-package ivy
  :ensure t
  :defer t
  :config
  (defun ivy-parent-dir ()
    (interactive)
    (when ivy--directory
      (ivy--cd (ivy--parent-dir (expand-file-name ivy--directory)))
      (ivy--exhibit)
      t))

  (defun colir--custom-blend-background (start next prevn face object)
    "Mix color only for `ivy-mode' faces."
    (put-text-property
     start next 'face
     (if-let ((background-prev (when (s-starts-with? "ivy" (symbol-name prevn))
                                 (face-background prevn))))
         (cons `(background-color
                 . ,(colir-blend
                     (colir-color-parse background-prev)
                     (colir-color-parse (face-background face nil t))))
               prevn)
       (list face prevn))
     object))

  (setq enable-recursive-minibuffers t
        ivy-height 20)

  (advice-add #'colir--blend-background :override #'colir--custom-blend-background)
  (advice-add #'ivy--highlight-default :before
              (lambda (_str)
                "Update `ivy--old-re' for Ivy caller using :dynamic-collection.
See `ivy--update-minibuffer', I think not updating `ivy--old-re' is intended.
But, In my case, it is not harm."
                (when (ivy-state-dynamic-collection ivy-last)
                  (setq ivy--old-re ivy-regex))))

  (ivy-mode 1))

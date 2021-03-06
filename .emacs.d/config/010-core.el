;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'counsel nil t)
  (require 'evil nil t)
  (require 'evil-search nil t)
  (require 'gcmh nil t)
  (require 'ivy nil t))

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
  (setq evil-flash-delay 1
        evil-undo-system 'undo-redo)

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

(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 1024 1024 2048))

  (add-hook 'after-save-hook
            (lambda ()
              (let ((gcmh-idle-delay 0.5))
                (gcmh-register-idle-gc)))
            100)

  (add-function :after after-focus-change-function
                (lambda ()
                  "Reclaim heap memory when fcousing out."
                  (let ((gcmh-idle-delay 0))
                    (gcmh-register-idle-gc)))
                '((depth . 100)))

  (gcmh-mode 1))

(use-package ivy
  :ensure t
  :defer t
  :init
  (defvar ivy-search-callers '(swiper counsel-rg counsel-projectile-rg lsp-ivy-workspace-symbol))

  :config
  (defun ivy-keyboard-quit ()
    (interactive)
    (minibuffer-keyboard-quit))

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

  (defun ivy--custom-add-face (str face)
    "Customize `ivy--add-face' for weight face"
    (let ((len (length str)))
      (condition-case nil
          (progn
            (let ((background (color-from face :background))
                  (foreground (color-from face :foreground))
                  (weight     (face-attribute face :weight))
                  (underline  (face-attribute face :underline))
                  (box        (face-attribute face :box)))
              (when background
                (add-face-text-property
                 0 len (list :background background) nil str))
              (when foreground
                (add-face-text-property
                 0 len (list :foreground foreground) nil str))
              (when weight
                (add-face-text-property
                 0 len (list :weight weight) nil str))
              (when underline
                (add-face-text-property
                 0 len (list :underline underline) nil str))
              (when box
                (add-face-text-property
                 0 len (list :box box) nil str))))
        (error
         (ignore-errors
           (font-lock-append-text-property 0 len 'face face str)))))
    str)

  (setq enable-recursive-minibuffers t
        ivy-height 20)

  (advice-add #'colir--blend-background :override #'colir--custom-blend-background)
  (advice-add #'ivy--add-face :override #'ivy--custom-add-face)
  (advice-add #'ivy--highlight-default :before
              (lambda (_str)
                "Update `ivy--old-re' for Ivy caller using :dynamic-collection.
See `ivy--update-minibuffer', I think not updating `ivy--old-re' is intended.
But, In my case, it is not harm."
                (when (ivy-state-dynamic-collection ivy-last)
                  (setq ivy--old-re ivy-regex))))

  (defvar colir-blend-cache (make-hash-table :test #'equal))

  (advice-add #'colir-blend :around
              (lambda (fn c1 c2)
                "Wrap `colir-blend' function to cache the result."
                (if-let ((cached-val (gethash (cons c1 c2) colir-blend-cache)))
                    cached-val
                  (let ((val (funcall fn c1 c2)))
                    (puthash (cons c1 c2) val colir-blend-cache)
                    val))))

  (advice-add #'ivy-switch-buffer-transformer :before-until
              (lambda (str)
                "To customize for `ivy-modified-buffer' face."
                (when-let ((buf (get-buffer str)))
                  (if (and (buffer-modified-p buf)
                           (buffer-file-name buf))
                      (ivy-append-face str 'ivy-modified-buffer)
                    ;; NOTE
                    ;;  I don't want to check whether the default behavior is changed or not on every update of `ivy'.
                    ;;  How to reuse a default behavior of `ivy-switch-buffer-transformer' function?
                    (let* ((mode (buffer-local-value 'major-mode buf))
                           (face (cdr (assq mode ivy-switch-buffer-faces-alist))))
                      (ivy-append-face str face))))))


  (ivy-mode 1))

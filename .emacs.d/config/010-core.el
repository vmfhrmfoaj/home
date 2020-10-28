;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package counsel
  :ensure t
  :defer t
  :config
  (setf (alist-get 'counsel-yank-pop       ivy-height-alist) 15)
  (setf (alist-get 'counsel-evil-registers ivy-height-alist) 15))

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

  (advice-add #'isearch-highlight :around
              (lambda (fn &rest args)
                "wrap with `ignore-errors'."
                (ignore-errors (apply fn args))))

  (evil-mode))

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
        ivy-height 15)

  (advice-add #'colir--blend-background :override #'colir--custom-blend-background)
  (advice-add #'ivy--highlight-default :before
              (lambda (_str)
                "Update `ivy--old-re' for Ivy caller using :dynamic-collection.
See `ivy--update-minibuffer', I think not updating `ivy--old-re' is intended.
But, In my case, it is not harm."
                (when (ivy-state-dynamic-collection ivy-last)
                  (setq ivy--old-re ivy-regex))))

  (ivy-mode 1))

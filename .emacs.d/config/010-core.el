(use-package evil
  :ensure t
  :init
  (defvar evil--auto-indent-region nil
    "TODO")

  (make-local-variable 'evil--auto-indent-region)

  (defun evil--auto-indent-region ()
    "TODO"
    (cond
     ((derived-mode-p 'prog-mode)
      (list (save-excursion
              (sp-backward-up-sexp)
              (point))
            (save-excursion
              (sp-up-sexp)
              (point))))
     (t nil)))

  (defun evil--auto-indent-save-pos ()
    "TODO"
    (setq evil--auto-indent-region (evil--auto-indent-region)))

  (defun evil--auto-indent ()
    "TODO"
    (unless (or evil-insert-vcount (null evil--auto-indent-region))
      (let ((m (make-marker)))
        (apply #'indent-region evil--auto-indent-region)
        (move-marker m nil nil))
      (setq evil--auto-indent-region nil)))

  (defun evil-jump-item-with-smartparens (&optional _count)
    "Improve `evil-jump-item-with' by using `show-smartparens-mode'"
    (let ((pair (sp-get-thing)))
      (when pair
        (when (s-blank? (plist-get pair :op))
          (save-excursion
            (skip-chars-backward "-_0-9A-Za-z")
            (setq pair (sp-get-thing))))
        (unless (s-blank? (plist-get pair :op))
          (sp-get pair
            (if (<= :beg (point) :beg-in)
                (goto-char :end-in)
              (goto-char :beg)))
          t))))

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
  (with-eval-after-load "smartparens"
    (add-hook 'evil-insert-state-entry-hook #'evil--auto-indent-save-pos)
    (add-hook 'evil-insert-state-exit-hook #'evil--auto-indent)
    (advice-add #'evil-jump-item :before-until #'evil-jump-item-with-smartparens))
  (evil-mode 1))

(use-package helm
  :ensure t
  :diminish ""
  :init
  (defun helm-bufferp (buf)
    "TODO"
    (when (and (bufferp buf)
               (string-match-p "\\*.*[Hh]elm.*\\*" (buffer-name buf)))
      t))

  (defun helm-resume-last-search-buffer ()
    "TODO"
    (interactive)
    (cond ((get-buffer "*helm ag results*")
           (switch-to-buffer-other-window "*helm ag results*"))
          ((get-buffer "*helm-ag*")
           (helm-resume "*helm-ag*"))
          (t
           (message "Not found any Helm search buffer"))))

  (defun helm-display-buffer-at-bottom (buffer &optional resume)
    "TODO"
    (let ((display-buffer-alist '(("\\*.*[Hh]elm.*\\*"
                                   (display-buffer-in-side-window)
                                   (inhibit-same-window . t)
                                   (side . bottom)))))
      (helm-default-display-buffer buffer resume)))

  (defun helm-persistent-action-display-window-for-neotree (&optional _)
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

  (defun helm-custom-mark-current-line (&optional _resumep _nomouse)
    "TODO"
    (helm--remove-custom-overlays)
    (let ((pattern (if (let ((case-fold-search t))
                         (->> (helm-get-current-source)
                              (assoc 'name)
                              (cdr)
                              (string-match-p (regexp-opt '("file" "project")))))
                       (file-name-base helm-pattern)
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
  (advice-add #'helm-persistent-action-display-window :before-until
              #'helm-persistent-action-display-window-for-neotree)
  (advice-add #'helm-initialize-overlays :after #'helm-custom-initialize-overlays)
  (advice-add #'helm-mark-current-line   :after #'helm-custom-mark-current-line)
  (helm-mode 1)
  (helm-autoresize-mode 1))

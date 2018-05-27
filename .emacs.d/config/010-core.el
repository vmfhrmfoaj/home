(use-package evil
  :ensure t
  :init
  (defvar evil-custom-forward-function nil
    "TODO")

  (defun evil-forward-symbol ()
    "TODO"
    (save-excursion
      (let* ((fwd-sym (or evil-custom-forward-function #'forward-symbol))
             (point (point))
             (end   (progn (funcall fwd-sym  1) (point)))
             (start (progn (funcall fwd-sym -1) (point))))
        (if (and (not (= start point end))
                 (<= start point end))
            (cons start end)
          (cons point (1+ point))))))

  :config
  (setq-default evil-symbol-word-search t)
  (put 'evil-symbol 'bounds-of-thing-at-point #'evil-forward-symbol)
  (global-subword-mode 1)
  (evil-mode 1))

(use-package helm-config
  :ensure helm
  :init
  (defun helm-resume-last-search-buffer ()
    "TODO"
    (interactive)
    (cond ((get-buffer "*helm ag results*")
           (switch-to-buffer-other-window "*helm ag results*"))
          ((get-buffer "*helm-ag*")
           (helm-resume "*helm-ag*"))
          (t
           (message "Not found any Helm search buffer"))))

  :config
  (setq helm-autoresize-min-height 25
        helm-autoresize-max-height 45
        helm-truncate-lines t)
  (helm-mode 1)
  (helm-autoresize-mode 1))

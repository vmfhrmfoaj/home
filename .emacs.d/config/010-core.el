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

(use-package helm
  :ensure t
  :defer t
  :diminish "Ⓗ"
  :commands (helm-make-source)
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

  (defun helm-display-buffer-at-bottom (buffer &optional resume)
    (let ((display-buffer-alist '(("\\*.*[Hh]elm.*\\*"
                                   (display-buffer-in-side-window)
                                   (inhibit-same-window . t)
                                   (side . bottom)
                                   (window-width  . 0.6)
                                   (window-height . 0.4)))))
      (helm-default-display-buffer buffer resume)))

  :config
  (require 'helm-config)
  (setq helm-autoresize-min-height 25
        helm-autoresize-max-height 45
        helm-display-header-line nil
        helm-display-function #'helm-display-buffer-at-bottom
        helm-split-window-inside-p t
        helm-truncate-lines t)
  (helm-mode 1)
  (helm-autoresize-mode 1))

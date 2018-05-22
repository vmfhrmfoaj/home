(use-package evil
  :ensure t
  :config
  (setq-default evil-symbol-word-search 'thing-at-point)
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

(use-package cc-mode
  :defer t
  :init
  (defn man-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (pop-to-buffer (man (concat thing "(3)")))))

  :config
  (setq c-default-style "linux")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local evil-lookup-func #'man-at-point)
              (c-toggle-auto-newline -1)
              (setq c-basic-offset 4))
            :append)
  (add-hook 'java-mode-hook
            (lambda ()
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines))))

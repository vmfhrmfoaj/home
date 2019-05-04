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
              (setq c-basic-offset 4)
              (setq-local evil-lookup-func #'man-at-point)
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines)
              (c-toggle-auto-newline -1))
            :append)
  (add-hook 'java-mode-hook
            (lambda ()
              (make-local-variable 'font-lock-extend-region-functions)
              (add-to-list 'font-lock-extend-region-functions #'font-lock-extend-region-wholelines))))

(use-package atomic-chrome
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'atomic-chrome-start-server))

(use-package linum-relative
  :ensure t
  :init
  (defun set-linum-rel-fmt-for-cur-file ()
    "TODO"
    (setq-local linum-relative-format
                (concat "%"
                        (-> (count-lines (point-min) (point-max))
                            (number-to-string)
                            (length)
                            (min 5)
                            (max 3)
                            (number-to-string))
                        "s")))

  :config
  (setq linum-relative-current-symbol "")
  (add-hook 'find-file-hook #'set-linum-rel-fmt-for-cur-file)
  (add-hook 'prog-mode-hook #'linum-relative-on))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package server
  :defer t
  :init
  (add-hook 'after-init-hook #'server-start))

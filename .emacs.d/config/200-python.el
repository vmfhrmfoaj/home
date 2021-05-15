;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package python
  :defer t
  :init
  (eval-when-compile (require 'python nil t))

  :config
  (advice-add #'run-python :after
              (lambda (&rest _)
                "To avoid confirming to terminate a process."
                (when-let ((p (get-buffer-process (current-buffer))))
                  (set-process-query-on-exit-flag  p nil)))))


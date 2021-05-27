;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'python nil t))

(use-package python
  :defer t
  :config
  (advice-add #'run-python :after
              (lambda (&rest _)
                "To avoid confirming to terminate a process."
                (when-let ((p (get-buffer-process (current-buffer))))
                  (set-process-query-on-exit-flag  p nil)))))


;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package python
  :defer t
  :config
  (advice-add #'run-python :after
              (lambda (&rest _)
                "To avoid confirming to terminate a process."
                (when-let ((p (get-buffer-process (current-buffer))))
                  (set-process-query-on-exit-flag  p nil)))))


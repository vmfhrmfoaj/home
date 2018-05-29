((emacs-lisp-mode . ((eval . (progn
                               (add-hook 'after-save-hook
                                         (lambda ()
                                           (when (string-match-p "[0-9]+-[^ \t\r\n]+\\.el" buffer-file-name)
                                             (let ((byte-compile-warnings nil))
                                               (byte-compile-file buffer-file-name))))))))))

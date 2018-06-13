((emacs-lisp-mode . ((eval . (progn
                               (add-hook 'after-save-hook
                                         (byte-compile
                                          (lambda ()
                                            (when (string-match-p "[0-9]+-[^ \t\r\n]+\\.el$" buffer-file-name)
                                              (let* ((sym (intern (->> (buffer-name)
                                                                       (s-replace-all '(("." . "_")))
                                                                       (concat "byte-compile--"))))
                                                     (fn-form `(defun ,sym ()
                                                                 (remove-hook 'kill-emacs-hook (quote ,sym))
                                                                 (let ((byte-compile-warnings nil))
                                                                   (byte-compile-file ,buffer-file-name)))))
                                                (if (not (fboundp sym))
                                                    (eval fn-form)
                                                  (remove-hook 'kill-buffer-hook sym :local)
                                                  (remove-hook 'kill-emacs-hook  sym))
                                                (add-hook 'kill-buffer-hook sym nil :local)
                                                (add-hook 'kill-emacs-hook  sym)))))))))))

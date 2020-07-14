(defvar home-dir (getenv "HOME"))

(setq HHKB? t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (let* ((setup-file (concat home-dir "/.script/setup"))
                   (cmd (concat "bash -c 'source " setup-file "; printenv PATH'")))
              (when-let ((path (and (file-exists-p setup-file)
                                    (string-trim (shell-command-to-string cmd)))))
                (setenv "PATH" path)
                (setq-default exec-path (split-string path ":"))))))

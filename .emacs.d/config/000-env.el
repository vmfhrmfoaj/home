;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(defvar home-dir (getenv "HOME"))

(setq HHKB? nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (let* ((setup-file (concat home-dir "/.script/setup"))
                   (env-vars '("PATH" "GOPATH" "GO111MODULE" "CARGO_ROOT_TARGET_DIR" "JAVA_HOME"))
                   (cmd (concat (apply #'concat "bash -c 'source " setup-file " >/dev/null 2>&1; printenv " (-interpose " " env-vars)) "'")))
              (--each
                  (->> cmd
                       (shell-command-to-string)
                       (string-trim)
                       (s-split "\n")
                       (-interleave env-vars)
                       (-partition 2))
                (let ((k (car  it))
                      (v (cadr it)))
                  (setenv k v)))
              (setq-default exec-path (split-string (getenv "PATH") ":")))))

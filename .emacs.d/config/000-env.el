;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(defvar home-dir (getenv "HOME"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (let* ((setup-file (concat home-dir "/.script/setup"))
                   (env-vars '("CARGO_HOME"
                               "CARGO_ROOT_TARGET_DIR"
                               "CARGO_ROOT_SAVED_DIR"
                               "EPYTHON"
                               "GO111MODULE"
                               "GOPATH"
                               "JAVA_HOME"
                               "JAVA_OPTS"
                               "LEIN_JVM_OPTS"
                               "PATH"))
                   (cmd (concat "bash -c 'source " setup-file " >/dev/null 2>&1"
                                "; for var in " (apply #'concat (-interpose " " env-vars))
                                "; do printenv ${var} || echo \"\"; done'")))
              (--each
                  (->> cmd
                       (shell-command-to-string)
                       (s-lines)
                       (-interleave env-vars)
                       (-partition 2))
                (let ((k (car  it))
                      (v (cadr it)))
                  (setenv k v)))
              (setq-default exec-path (split-string (getenv "PATH") ":")))))

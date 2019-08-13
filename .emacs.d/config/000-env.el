(defvar home-dir (getenv "HOME"))

(let ((emacs-src-path (concat home-dir "/Desktop/Open_Sources/emacs")))
  (when (file-exists-p emacs-src-path)
    (setq source-directory emacs-src-path)))
(setq HHKB? t)

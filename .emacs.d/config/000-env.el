(let ((emacs-src-path (concat (getenv "HOME") "/Desktop/Open_Sources/emacs")))
  (setq HHKB? nil)
  (when (file-exists-p emacs-src-path)
    (setq source-directory emacs-src-path)))

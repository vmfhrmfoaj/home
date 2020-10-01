;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package cider
  :ensure t
  :defer t
  :config
  (defun cider-switch-to-releated-repl-buffer (&optional set-namespace)
    (interactive "P")
    (if (cider-connected-p)
        (let* ((repl-type (cider-connection-type-for-buffer))
               (root (clojure-project-root-path))
               (all-repl-bufs (cider-connections))
               (prj-repl-bufs (--filter (with-current-buffer it
                                          (string-equal root (clojure-project-root-path)))
                                        all-repl-bufs))
               (buffer (--first (with-current-buffer it
                                  (string-equal repl-type cider-repl-type))
                                (or prj-repl-bufs all-repl-bufs))))
          (cider--switch-to-repl-buffer (or buffer
                                            (-first-item prj-repl-bufs)
                                            (-first-item all-repl-bufs))
                                        set-namespace))
      (message "Cider didn't start")))

  (defun cider-hard-restart (&optional repl)
    (interactive)
    (let* ((repl (or repl
                     (sesman-browser-get 'object)
                     (cider-current-repl nil 'ensure))))
      (cider-quit repl)
      (cider-jack-in nil)))

  (defun cider-find-var-at-point ()
    (interactive)
    (if (cider-connected-p)
        (cider-find-var nil (cider-symbol-at-point))
      (dumb-jump-go)))

  (defun cider-switch-to-last-clj-buf ()
    (interactive)
    (let ((display-fn (if cider-repl-display-in-current-window
                          #'pop-to-buffer-same-window
                        #'pop-to-buffer))
          (mode-vals (if (string= "clj" cider-repl-type)
                         '((clojure-mode . 2) (clojurescript-mode . 1))
                       '((clojurescript-mode . 2) (clojure-mode . 1)))))
      (-some->> (or (projectile-project-buffers) (buffer-list))
        (sort-buffer-by-visit-time)
        (--sort (let ((itv    (with-current-buffer it    (alist-get major-mode mode-vals 0)))
                      (otherv (with-current-buffer other (alist-get major-mode mode-vals 0))))
                  (> itv otherv)))
        (-first-item)
        (funcall display-fn))))

  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (setq-local font-lock-fontify-region-function #'font-lock-default-fontify-region)
              (eldoc-mode 1)))

  (advice-add #'cider-restart :around
              (lambda (fn &rest args)
                "Wrap `cider-restart' to keep current namespace of REPL."
                (let ((cur-ns (cider-current-ns)))
                  (apply fn args)
                  (cider-repl-set-ns cur-ns)))))

(use-package cider-mode
  :defer t
  :config
  (defun cider-doc-at-point ()
    (interactive)
    (cider-doc-lookup (cider-symbol-at-point)))

  (add-hook 'cider-connected-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point))))

(use-package cider-repl
  :defer t
  :config
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'cider-doc-at-point)
              (eldoc-mode 1))))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (when (fboundp 'eldoc-refresh)
                (add-hook 'evil-insert-state-entry-hook
                          (lambda ()
                            (when (timerp eldoc-timer)
                              (cancel-timer eldoc-timer)
                              (setq eldoc-timer nil))
                            (run-at-time 0.1 nil (-partial #'call-interactively #'eldoc-refresh)))
                          nil :local))
              (setq-local font-lock-extend-region-functions
                          (remove 'clojure-font-lock-extend-region-def
                                  font-lock-extend-region-functions))
              (flycheck-mode))
            :append))

(use-package edn
  :ensure t
  :defer t
  :config
  (defun edn-raw-p (raw)
    (and (consp raw)
         (eq 'edn-raw (car raw))))

  (defun edn-raw (raw)
    (format "%s" (cdr raw)))

  (defun edn-reader-macro-p (reader-macro)
    (and (listp reader-macro)
         (eq 'edn-reader-macro (car reader-macro))))

  (defun edn-reader-macro (reader-macro)
    (let ((reader-macro (cadr reader-macro))
          (form (caddr reader-macro)))
      (concat reader-macro (edn-print-string form))))

  (edn-add-writer #'edn-raw-p #'edn-raw)
  (edn-add-writer #'edn-reader-macro-p #'edn-reader-macro))

(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)

(use-package helm-cider-cheatsheet
  :ensure helm-cider
  :defer t)

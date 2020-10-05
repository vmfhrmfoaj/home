;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package counsel-projectile
  :ensure t
  :defer t
  :config
  (setq counsel-projectile-remove-current-buffer t))

(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :commands (projectile-project-root
             projectile-project-buffers)
  :init
  (defun projectile-switch-to-previous-buffer ()
    "TODO"
    (interactive)
    (condition-case nil
        (let ((cur-proj-root (or (projectile-project-root)
                                 (concat (s-chop-suffix "/" home-dir) "/"))))
          (->> (buffer-list)
               (--filter (with-current-buffer it
                           (let ((proj-root (or (projectile-project-root)
                                                (concat (s-chop-suffix "/" home-dir) "/"))))
                             (string= cur-proj-root proj-root))))
               (switch-to-previous-buffer-in)))
      (error (switch-to-previous-buffer-in (buffer-list)))))

  (defun projectile-kill-buffer (&optional buf)
    "TODO"
    (interactive)
    (let ((buf (or buf (current-buffer))))
      (projectile-switch-to-previous-buffer)
      (kill-buffer buf)))

  (defun projectile-switch-latest-open-project ()
    "TODO"
    (interactive)
    (let ((cur-proj-root (or (projectile-project-root)
                             (concat (s-chop-suffix "/" home-dir) "/"))))
      (-some->> (buffer-list)
        (--remove (with-current-buffer it
                    (let ((proj-root (or (projectile-project-root)
                                         (concat (s-chop-suffix "/" home-dir) "/"))))
                      (string= cur-proj-root proj-root))))
        (switch-to-previous-buffer-in))))

  (defun projectile-action-for-custom-switch-open-project ()
    "A `projectile' action for `projectile-custom-switch-open-project'."
    (let* ((cur-proj-root (projectile-project-root))
           (buf (-some->> (buffer-list)
                  (--filter (with-current-buffer it
                              ;; NOTE
                              ;;  `projectile-switch-open-project' will overwrite `default-directory' variable.
                              (let* ((default-directory (or (-some-> (buffer-file-name) (file-name-directory)) ""))
                                     (proj-root (or (projectile-project-root)
                                                    (concat (s-chop-suffix "/" home-dir) "/"))))
                                (string= cur-proj-root proj-root))))
                  (switch-to-previous-buffer-in))))
      (unless buf
        (projectile-find-file))))

  (defun projectile-custom-switch-open-project (&optional arg)
    "TODO"
    (interactive)
    (let ((projectile-switch-project-action #'projectile-action-for-custom-switch-open-project))
      (projectile-switch-open-project)))

  :config
  (defun projectile-project-files-custom-filter (files)
    "TODO"
    (-if-let (regex (-some->> (projectile-paths-to-ignore)
                      (--map (->> it
                                  (s-chop-prefix (file-truename (projectile-project-root)))
                                  (concat "^")))
                      (append (projectile-patterns-to-ignore))
                      (-interpose "\\|")
                      (apply #'concat)))
        (-remove (-partial #'string-match-p regex) files)
      files))

  (defun projectile-custom-project-name (project-root)
    "TODO"
    (if (string= home-dir (s-chop-suffix "/" project-root))
        "home"
      (file-name-nondirectory (directory-file-name project-root))))

  (defun projectile-kill-new-buffer-file-name ()
    "TODO"
    (interactive)
    (-when-let (file-name (buffer-file-name))
      (message (kill-new (-if-let (root (projectile-project-root))
                             (s-chop-prefix root file-name)
                           file-name)))))

  (defun projectile-custom-project-root (&optional dir)
    "Fix for improving the performance of `projectile-project-root'."
    ;; the cached value will be 'none in the case of no project root (this is to
    ;; ensure it is not reevaluated each time when not inside a project) so use
    ;; cl-subst to replace this 'none value with nil so a nil value is used
    ;; instead
    (let ((dir (or dir default-directory)))
      ;; Back out of any archives, the project will live on the outside and
      ;; searching them is slow.
      (when (and (fboundp 'tramp-archive-file-name-archive)
                 (tramp-archive-file-name-p dir))
        (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
      (cl-subst nil 'none
                ;; The `is-local' and `is-connected' variables are
                ;; used to fix the behavior where Emacs hangs
                ;; because of Projectile when you open a file over
                ;; TRAMP. It basically prevents Projectile from
                ;; trying to find information about files for which
                ;; it's not possible to get that information right
                ;; now.
                (or (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                          (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                      (when (or is-local is-connected)
                        (cl-some
                         (lambda (func)
                           (let* ((cache-key (format "%s-%s" func dir))
                                  (cache-value (gethash cache-key projectile-project-root-cache :no-matched)))
                             (if (eq :no-matched cache-value)
                                 (let ((value (funcall func (file-truename dir))))
                                   (puthash cache-key value projectile-project-root-cache)
                                   value)
                               cache-value)))
                         projectile-project-root-files-functions)))
                    ;; set cached to none so is non-nil so we don't try
                    ;; and look it up again
                    'none))))

  (defun projectile-custom-kill-buffers (&optional proj-root)
    "Kill all project buffers without exception"
    (interactive)
    (-when-let (proj-root (or proj-root (projectile-project-root)))
      (let ((alive-buf-list nil))
        (--each
            (--filter (with-current-buffer it
                        (when-let ((cur-proj-root (projectile-project-root)))
                          (string-equal proj-root cur-proj-root)))
                      (buffer-list))
          (let ((confirm-kill-processes nil))
            (kill-buffer it))))))

  (defun projectile-custom-open-projects ()
    "Return a list of all open projects.
An open project is a project with any open buffers expect `mini-buffer'."
    (->> (buffer-list)
         (-remove #'minibufferp)
         (--map (with-current-buffer it
                  (when-let ((proj (projectile-project-p)))
                    (abbreviate-file-name proj))))
         (-distinct)
         (-non-nil)))

  (setq projectile-completion-system 'ivy
        projectile-enable-cachig t
        projectile-project-name-function #'projectile-custom-project-name)

  (add-hook 'find-file-hook
            (lambda ()
              (setq-local dumb-jump-project
                          (-> buffer-file-name
                              (file-name-directory)
                              (projectile-project-root)))))

  (advice-add #'projectile-project-files :filter-return #'projectile-project-files-custom-filter)
  (advice-add #'projectile-project-root :override #'projectile-custom-project-root)

  (advice-add #'projectile-project-buffer-p :before-while
              (lambda (buf root) "filter nil to avoid type error" root))

  (advice-add #'projectile-project-name :filter-return
              (lambda (proj-name)
                "To cache project name."
                (setq-local projectile-project-name proj-name)))

  (advice-add #'projectile-kill-buffers :override #'projectile-custom-kill-buffers)
  (advice-add #'projectile-open-projects :override #'projectile-custom-open-projects)

  (projectile-mode 1)

  ;; NOTE
  ;;  Switch the default project
  (let ((projectile-switch-project-action (lambda (&rest _))))
    (projectile-switch-project-by-name "~/")))

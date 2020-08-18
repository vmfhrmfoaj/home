;; -*- lexical-binding: t; -*-

(eval-and-compile
  (load-file "~/.emacs.d/config/func.el"))

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

  (defvar helm-source-project-buffers-list nil)

  (defun helm-project-buffers-list ()
    "Customize `helm-buffers-list' for `projectile'"
    (interactive)
    (unless helm-source-project-buffers-list
      (setq helm-source-project-buffers-list
            (helm-make-source "Project Buffers" 'helm-source-buffers
              :buffer-list (lambda ()
                             (->> (or (projectile-project-buffers)
                                      (buffer-list))
                                  (sort-buffer-by-visit-time)
                                  (-rotate -1)
                                  (-map #'buffer-name))))))
    (helm :sources 'helm-source-project-buffers-list
          :buffer "*helm project buffers*"
          :keymap helm-buffer-map
          :truncate-lines helm-buffers-truncate-lines
          :left-margin-width helm-buffers-left-margin-width))

  (defvar helm-source-project-find-files nil)

  (defface helm-ff-file-dir
    '((t (:inherit helm-ff-file)))
    "TODO")

  (defface helm-ff-executable-dir
    '((t (:inherit helm-ff-executable)))
    "TODO")

  (defun helm-project-find-files ()
    "Customize `helm-find-files' for `projectile'"
    (interactive)
    (unless helm-source-project-find-files
      (setq helm-source-project-find-files
            (helm-make-source "Project Files" 'helm-source-sync
              :candidates
              (lambda ()
                (when-let ((proj-root (projectile-project-root)))
                  (-some->> (projectile-project-files proj-root)
                    (--map (let* ((faces (cond
                                          ((file-executable-p (concat proj-root it))
                                           '(helm-ff-executable-dir helm-ff-executable helm-ff-file-extension))
                                          (t
                                           '(helm-ff-file-dir       helm-ff-file       helm-ff-file-extension))))
                                  (dir-name  (-some-> it (file-name-directory) (propertize 'face (car  faces))))
                                  (file-name (-some-> it (file-name-base)      (propertize 'face (cadr faces))))
                                  (ext-name  (-some-> it (file-name-extension) (->> (concat ".")) (propertize 'face (caddr faces))))
                                  (it (propertize (concat proj-root
                                                          dir-name
                                                          file-name
                                                          ext-name)
                                                  'proj-path proj-root)))
                             it)))))
              :real-to-display
              (lambda (c)
                (-when-let (len (-some->> c (get-text-property 0 'proj-path) (length)))
                  (substring c len)))

              :display-to-real
              (lambda (c)
                (if-let ((proj-root (get-text-property 0 'root-path c)))
                    (concat proj-root "/" c)
                  c))

              :action #'find-file)))
    (helm :sources 'helm-source-project-find-files
          :buffer "*helm project files*"))

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
    (-when-let (proj-root (projectile-ensure-project (or proj-root (projectile-project-root))))
      (--each
          (--filter (with-current-buffer it
                      (-when-let (cur-proj-root (projectile-project-root))
                        (string-equal proj-root cur-proj-root)))
                    (buffer-list))
        (let ((confirm-kill-processes nil))
          (kill-buffer it)))))

  (setq projectile-completion-system 'helm
        projectile-enable-cachig t
        projectile-project-name-function #'projectile-custom-project-name
        projectile-switch-project-action #'helm-project-find-files)

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

  (projectile-mode 1)

  ;; NOTE
  ;;  Switch the default project
  (let ((projectile-switch-project-action (lambda (&rest _))))
    (projectile-switch-project-by-name "~/")))

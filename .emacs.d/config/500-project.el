;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'counsel-projectile nil t)
  (require 'projectile nil t))

(use-package counsel-projectile
  :ensure t
  :defer t
  :config
  (setq counsel-projectile-remove-current-buffer t))

(use-package projectile
  :ensure t
  :defer t
  :diminish ""
  :init
  (defun projectile-switch-to-previous-buffer ()
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
    (interactive)
    (let ((buf (or buf (current-buffer))))
      (projectile-switch-to-previous-buffer)
      (kill-buffer buf)))

  (defun projectile-switch-latest-open-project ()
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

  (defun projectile-custom-switch-open-project ()
    (interactive)
    (let ((projectile-switch-project-action #'projectile-action-for-custom-switch-open-project))
      (projectile-switch-open-project)))

  :config
  (defun projectile-project-files-custom-filter (files)
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
    (if (string= home-dir (s-chop-suffix "/" project-root))
        "home"
      (file-name-nondirectory (directory-file-name project-root))))

  (defun projectile-kill-new-buffer-file-name ()
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
      (--each
          (--filter (with-current-buffer it
                      (when-let ((cur-proj-root (projectile-project-root)))
                        (string-equal proj-root cur-proj-root)))
                    (buffer-list))
        (let ((confirm-kill-processes nil))
          (kill-buffer it)))))

  (defun projectile-custom-open-projects ()
    (->> (buffer-list)
         (-remove #'minibufferp)
         (--map (with-current-buffer it
                  (when-let ((proj (projectile-project-p)))
                    (abbreviate-file-name proj))))
         (-distinct)
         (-non-nil)))

  (defun projectile-custom-project-vcs (&optional project-root)
    (or project-root (setq project-root (projectile-project-root)))
    (cond
     ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
     ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
     ((projectile-file-exists-p (expand-file-name ".fslckout" project-root)) 'fossil)
     ((projectile-file-exists-p (expand-file-name "_FOSSIL_" project-root)) 'fossil)
     ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
     ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
     ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
     ((-some-> (projectile-locate-dominating-file project-root ".git")      (string= project-root)) 'git)
     ((-some-> (projectile-locate-dominating-file project-root ".hg")       (string= project-root)) 'hg)
     ((-some-> (projectile-locate-dominating-file project-root ".fslckout") (string= project-root)) 'fossil)
     ((-some-> (projectile-locate-dominating-file project-root "_FOSSIL_")  (string= project-root)) 'fossil)
     ((-some-> (projectile-locate-dominating-file project-root ".bzr")      (string= project-root)) 'bzr)
     ((-some-> (projectile-locate-dominating-file project-root "_darcs")    (string= project-root)) 'darcs)
     ((-some-> (projectile-locate-dominating-file project-root ".svn")      (string= project-root)) 'svn)
     (t 'none)))

  (defun projectile-custom-project-buffer-p (buffer project-root)
    "customize for `directory-abbrev-alist'"
    (with-current-buffer buffer
      (and (not (string-prefix-p " " (buffer-name buffer)))
           (not (projectile-ignored-buffer-p buffer))
           default-directory
           (or projectile-project-root
               (string-equal (file-remote-p default-directory)
                             (file-remote-p project-root)))
           (not (string-match-p "^http\\(s\\)?://" default-directory))
           (string-prefix-p (abbreviate-file-name project-root)
                            (abbreviate-file-name (file-truename (or projectile-project-root
                                                                     default-directory)))
                            (eq system-type 'windows-nt)))))

  (defun projectile-add-buffer-to-project ()
    (interactive)
    (let* ((proj-root (projectile-project-root))
           (proj-name (projectile-project-name proj-root)))
      (when-let ((buf (->> (buffer-list)
                           (-map #'buffer-name)
                           (completing-read "Add a buffer to the current project:")
                           (get-buffer))))
        (with-current-buffer buf
          (dolist (func projectile-project-root-functions)
            (let ((key (format "%s-%s" func default-directory)))
              (remhash key projectile-project-root-cache)))
          (setq-local projectile-project-name proj-name
                      projectile-project-root proj-root)))))

  (setq projectile-completion-system 'ivy
        projectile-enable-cachig t
        projectile-project-name-function #'projectile-custom-project-name)

  (add-hook 'find-file-hook
            (lambda ()
              (setq-local dumb-jump-project
                          (-> buffer-file-name
                              (file-name-directory)
                              (projectile-project-root)))))


  (advice-add #'projectile-kill-buffers  :override #'projectile-custom-kill-buffers)
  (advice-add #'projectile-open-projects :override #'projectile-custom-open-projects)
  (advice-add #'projectile-project-buffer-p :override #'projectile-custom-project-buffer-p)
  (advice-add #'projectile-project-files :filter-return #'projectile-project-files-custom-filter)
  (advice-add #'projectile-project-root :override #'projectile-custom-project-root)
  (advice-add #'projectile-project-vcs  :override #'projectile-custom-project-vcs)
  (advice-add #'projectile-project-buffer-p :before-while
              (lambda (_buf root)
                "filter nil to avoid type error"
                root))
  (advice-add #'projectile-project-name :filter-return
              (lambda (proj-name)
                "To cache project name."
                (setq-local projectile-project-name proj-name)))

  (projectile-mode 1)

  ;; NOTE
  ;;  Switch the default project
  (let ((projectile-switch-project-action (lambda (&rest _))))
    (projectile-switch-project-by-name "~/")))

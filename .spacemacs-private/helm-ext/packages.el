;;; packages.el --- helm-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst helm-ext-packages
  '(dumb-jump
    helm
    helm-ag
    helm-projectile
    (minibuffer :location built-in)))

(defun helm-ext/post-init-dumb-jump ()
  (use-package helm
    :defer t
    :config
    (byte-compile #'helm-dump-jump--insert-file)
    (byte-compile #'helm-dump-jump--actions)
    (byte-compile #'helm-dump-jump--persistent-action)

    (setq dumb-jump-git-grep-cmd "git grep --full-name")
    (defvar helm-dump-jump--actions
      (helm-make-actions
       "Open file"              (-partial #'helm-dump-jump--action #'find-file)
       "Open file other window" (-partial #'helm-dump-jump--action #'find-file-other-window)))
    (advice-add #'dumb-jump-handle-results :before
                (byte-compile
                 (lambda (results cur-file proj-root ctx-type look-for use-tooltip prefer-external)
                   (setq helm-dumb-jump--keyword look-for))))
    (advice-add #'dumb-jump-prompt-user-for-choice :before-until
                (byte-compile
                 (lambda (proj results)
                   (when (eq 'helm dumb-jump-selector)
                     (setq helm-dumb-jump--proj proj)
                     (let* ((proj-regex (concat "^" (regexp-quote proj) "/*"))
                            (paths (->> results
                                        (--map (plist-get it :path))
                                        (--map (s-replace-regexp proj-regex "" it))
                                        (--map (propertize it 'face 'helm-moccur-buffer))))
                            (lines (->> results
                                        (--map (plist-get it :line))
                                        (--map (number-to-string it))
                                        (--map (propertize it 'face 'helm-grep-lineno))))
                            (ctxs  (->> results
                                        (--map (plist-get it :context))
                                        (--map (s-trim it))
                                        (--map (s-split helm-dumb-jump--keyword it))
                                        (--map (-interpose (propertize helm-dumb-jump--keyword 'face 'helm-match) ij))
                                        (--map (apply #'concat " " it))))
                            (candidates (->> (-zip paths lines ctxs)
                                             (--map (-interpose ":" it))
                                             (--map (apply #'concat it)))))
                       (helm :sources
                             (helm-build-sync-source "Dump Jump"
                               :candidates candidates
                               :action helm-dump-jump--actions
                               :persistent-action #'helm-dump-jump--persistent-action)
                             :buffer "*helm-dumb-jump*"))
                     t))))))

(defun helm-ext/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (define-key helm-map (kbd "C-n") #'helm-next-source)
    (define-key helm-map (kbd "C-p") #'helm-previous-source)
    (define-key helm-comp-read-map  (kbd "C-h") #'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") #'delete-backward-char)
    (define-key helm-read-file-map  (kbd "C-h") #'delete-backward-char)
    (setq helm-autoresize-min-height 20
          helm-autoresize-max-height 45
          helm-truncate-lines t)
    (when (or dotspacemacs-fullscreen-at-startup
              dotspacemacs-fullscreen-use-non-native
              dotspacemacs-maximized-at-startup)
      (setq helm-display-function #'helm-default-display-buffer
            helm-split-window-in-side-p t
            pupo-split-active-window t))
    (add-hook 'helm-after-action-hook
              (byte-compile
               (lambda ()
                 (ignore-errors
                   (recenter)))))
    (setq helm-autoresize-max-height--save nil)
    (setq helm-autoresize-max-height-for-swoop 65)
    (add-hook 'helm-minibuffer-set-up-hook
              (lambda ()
                (when (and (boundp 'helm-swoop-buffer)
                           (string-equal helm-swoop-buffer (or helm-buffer "")))
                  (setq helm-autoresize-max-height--save helm-autoresize-max-height
                        helm-autoresize-max-height helm-autoresize-max-height-for-swoop)
                  (helm--autoresize-hook))))
    (let ((f (lambda ()
               (when helm-autoresize-max-height--save
                 (setq helm-autoresize-max-height helm-autoresize-max-height--save
                       helm-autoresize-max-height--save nil)))))
      (add-hook 'helm-exit-minibuffer-hook f)
      (add-hook 'helm-quit-hook f))))

(defun helm-ext/post-init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
    (setq helm-ag-use-emacs-lisp-regexp t)
    (when (and (executable-find "rg")
               (> (--find-index (string-equal it "ag") dotspacemacs-search-tools)
                  (--find-index (string-equal it "rg") dotspacemacs-search-tools)))
      (setq helm-ag-base-command "rg --color never --no-heading"))
    (advice-add #'helm-ag--marked-input :around
                (lambda (of escape)
                  (let ((res (funcall of escape)))
                    (if (and escape res)
                        (regexp-quote res)
                      res))))))

(defun helm-ext/post-init-helm-projectile ()
  (use-package helm-projectile
    :defer t
    :config
    (define-key helm-projectile-find-file-map
      (kbd "C-h") #'delete-backward-char)
    (define-key (assoc 'keymap helm-source-projectile-dired-files-list)
      (kbd "C-h") #'delete-backward-char)))

(defun helm-ext/init-minibuffer ()
  (use-package minibuffer
    :defer t
    :config
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (local-set-key (kbd "C-d")   #'delete-char)
                (local-set-key (kbd "C-h")   #'delete-backward-char)
                (local-set-key (kbd "S-SPC") #'toggle-input-method)
                (smartparens-mode t)))))

;;; packages.el ends here

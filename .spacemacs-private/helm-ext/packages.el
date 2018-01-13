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
  (use-package dumb-jump
    :defer t
    :config
    (byte-compile #'helm-dump-jump--insert-file)
    (byte-compile #'helm-dump-jump--actions)
    (byte-compile #'helm-dump-jump--persistent-action)

    (with-eval-after-load "helm"
      (setq dumb-jump-git-grep-cmd "git grep --full-name")
      (defvar helm-dump-jump--actions
        (helm-make-actions
         "Open file"              (-partial #'helm-dump-jump--action #'find-file)
         "Open file other window" (-partial #'helm-dump-jump--action #'find-file-other-window)))
      (advice-add #'dumb-jump-get-results :filter-return
                  (byte-compile
                   (lambda (info)
                     (setq helm-dumb-jump--proj (plist-get info :root)
                           helm-dumb-jump--keyword (plist-get info :symbol))
                     info)))
      (advice-add #'dumb-jump--result-follow :override
                  (byte-compile
                   (lambda (result &optional use-tooltip proj)
                     (let* ((path (concat helm-dumb-jump--proj "/" (plist-get result :path)))
                            (line (plist-get result :line))
                            (pos (->> (plist-get result :context)
                                      (s-split helm-dumb-jump--keyword)
                                      (first)
                                      (length))))
                       (dumb-jump-goto-file-line path line pos)))))
      (advice-add #'dumb-jump-prompt-user-for-choice :before-until
                  (byte-compile
                   (lambda (proj results)
                     (when (eq 'helm dumb-jump-selector)
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
                                          (--map (s-split (regexp-quote helm-dumb-jump--keyword) it))
                                          (--map (-interpose (propertize helm-dumb-jump--keyword 'face 'helm-match) it))
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
                       t)))))))

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
      (add-hook 'helm-quit-hook f)))
  (use-package helm-org
    :defer t
    :config
    (advice-add #'helm-org-completing-read-tags :around
                (byte-compile
                 (lambda (fn &rest args)
                   (let ((completion-ignore-case t))
                     (apply fn args)))))))

(defun helm-ext/post-init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
    (setq helm-ag-use-emacs-lisp-regexp t)
    (advice-add #'helm-ag--propertize-candidates :before-until
                (byte-compile
                 (lambda (input)
                   (not (safe-regexp? input)))))))

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

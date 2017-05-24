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
  '(helm
    helm-projectile
    (minibuffer :location built-in)))

(defun helm-ext/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (define-key helm-map (kbd "C-n") #'helm-next-source)
    (define-key helm-map (kbd "C-p") #'helm-previous-source)
    (setq helm-truncate-lines t
          helm-autoresize-min-height 35)
    (helm-autoresize-mode 1))
  (use-package helm-mode
    :ensure helm
    :defer t
    :config
    (define-key helm-comp-read-map (kbd "C-h") #'delete-backward-char)))

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
                (local-set-key (kbd "C-h")   #'backward-delete-char)
                (local-set-key (kbd "S-SPC") #'toggle-input-method)
                (smartparens-mode 1)))))

;;; packages.el ends here

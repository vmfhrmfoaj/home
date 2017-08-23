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
    helm-ag
    helm-projectile
    (minibuffer :location built-in)))

(defun helm-ext/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (define-key helm-map (kbd "C-n") #'helm-next-source)
    (define-key helm-map (kbd "C-p") #'helm-previous-source)
    (setq helm-autoresize-min-height 20
          helm-autoresize-max-height 45
          helm-display-function #'helm-default-display-buffer
          helm-split-window-in-side-p t
          helm-truncate-lines t
          pupo-split-active-window t)
    (add-hook 'helm-after-action-hook
              (byte-compile
               (lambda ()
                 (ignore-errors
                   (recenter))))))

  (use-package helm-mode
    :defer t
    :config
    (define-key helm-comp-read-map (kbd "C-h") #'delete-backward-char)))

(defun helm-ext/post-init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
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
                (local-set-key (kbd "C-h")   #'backward-delete-char)
                (local-set-key (kbd "S-SPC") #'toggle-input-method)
                (smartparens-mode 1)))))

;;; packages.el ends here

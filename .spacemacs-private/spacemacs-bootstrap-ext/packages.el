;;; packages.el --- spacemacs-bootstrap-ext layer packages file for Spacemacs.
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

(defconst spacemacs-bootstrap-ext-packages
  '(evil
    which-key))

(defun spacemacs-bootstrap-ext/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (define-key evil-ex-map (kbd "C-h") #'delete-backward-char)
    (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
    (define-key evil-motion-state-map (kbd "g S-<kp-subtract>") #'evil-last-non-blank)
    (define-key evil-read-key-map (kbd "<S-kp-divide>") "\\")
    (define-key evil-read-key-map (kbd "<S-kp-subtract>") "_")
    (define-key evil-read-key-map (kbd "<S-kp-add>") "=")
    (add-hook 'evil-normal-state-entry-hook #'auto-indent)
    (advice-add #'open-line :after #'auto-indent)
    (advice-add #'evil-insert-resume :after
                (let ((byte-compile-warnings nil)
                      (f (lambda (&rest _)
                           (recenter))))
                  (byte-compile f)))
    (dolist (fn '(evil-change evil-delete evil-join evil-paste-after))
      (advice-add fn :after #'auto-indent))))

(defun spacemacs-bootstrap-ext/post-init-which-key ()
  (use-package which-key
    :defer
    :config
    (setq which-key-dont-use-unicode t)))

;;; packages.el ends here

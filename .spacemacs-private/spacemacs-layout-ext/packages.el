;;; packages.el --- spacemacs-layout-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst spacemacs-layout-ext-packages
  '(persp-mode))

(defun spacemacs-layout-ext/post-init-persp-mode ()
  (use-package persp-mode
    :defer t
    :config
    (advice-add #'persp-kill :before
                (lambda (&rest _)
                  "Kill all buffer in the layout."
                  (-when-let (persp (get-current-persp))
                    (dolist (buf (aref persp 2))
                      (kill-buffer-if-not-modified buf)))))))

;;; packages.el ends here

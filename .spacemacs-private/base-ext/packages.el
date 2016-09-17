;;; packages.el --- base-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: KimJinseop <Jinseop@KimJinseops-iMac.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst base-ext-packages
  '(
    ediff
    ))

(defun base-ext/post-init-ediff ()
  (use-package ediff
    :config
    (add-hook 'ediff-before-setup-hook #'spacemacs/toggle-maximize-frame-on)
    (advice-add #'ediff-quit :after (lambda (&rest _)
                                      (spacemacs/toggle-maximize-frame-off)))))

;;; packages.el ends here

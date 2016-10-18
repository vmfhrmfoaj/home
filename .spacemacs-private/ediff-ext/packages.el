;;; packages.el --- ediff-ext layer packages file for Spacemacs.
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

(defconst ediff-ext-packages
  '(ediff))

(defun ediff-ext/post-init-ediff ()
  (use-package ediff
    :defer t
    :config
    (setq ediff-split-window-function
          ;; NOTE
          ;; prevent to calculate the width of the window
          ;;  in `ediff-setup-windows-plain-compare' function.
          (lambda (&rest _)
            (split-window-right)))
    (advice-add #'ediff-setup :before
                (lambda (&rest _)
                  (spacemacs/toggle-maximize-frame-on)))
    (advice-add #'ediff-quit :after
                (lambda (&rest _)
                  (spacemacs/toggle-maximize-frame-off)))))

;;; packages.el ends here

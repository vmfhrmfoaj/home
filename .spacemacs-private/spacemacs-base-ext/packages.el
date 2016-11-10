;;; packages.el --- spacemacs-base-ext layer packages file for Spacemacs.
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

(defconst spacemacs-base-ext-packages
  '((ediff :location built-in)))

(defun spacemacs-base-ext/post-init-ediff ()
  (use-package ediff
    :defer t
    :config
    ;; NOTE
    ;; prevent to calculate the width of the window
    ;;  in `ediff-setup-windows-plain-compare' function.
    (setq ediff-exclude-modes '(golden-ratio-mode)
          ediff-split-window-function (lambda (&rest _)
                                        (split-window-right)))
    (advice-add #'ediff-setup :before
                (lambda (&rest _)
                  (setq ediff-exclude-mode-status (-map #'symbol-value ediff-exclude-modes))
                  (disable-modes ediff-exclude-modes)
                  (unless (cdr (assoc 'fullscreen (frame-parameters)))
                    (spacemacs/toggle-maximize-frame-on))))
    (advice-add #'ediff-quit :after
                (lambda (&rest _)
                  (restore-modes ediff-exclude-modes ediff-exclude-mode-status)
                  (when (eq 'maximized (cdr (assoc 'fullscreen (frame-parameters))))
                    (spacemacs/toggle-maximize-frame-off))))))

;;; packages.el ends here

;;; packages.el --- java-ext layer packages file for Spacemacs.
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

(defconst java-ext-packages
  '(eclim))

(defun java-ext/post-init-eclim ()
  (use-package eclim
    :defer t
    :config
    (advice-add #'eclim-problems-advice-other-window     :override (byte-compile (lambda (&rest _))))
    (advice-add #'eclim-problems-advice-switch-to-buffer :override (byte-compile (lambda (&rest _))))
    (advice-add #'eclim-java-find-declaration :around
                (byte-compile
                 (lambda (of)
                   (if (eclim--connected-p)
                       (funcall-interactively of)
                     (dumb-jump-go)))))))

;;; packages.el ends here

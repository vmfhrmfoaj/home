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
    (setq eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse")
          eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))

  (use-package eclim-problems
    :defer t
    :config
    (ad-remove-advice #'switch-to-buffer 'after 'eclim-problems-highlight-switch-to-buffer)
    (ad-remove-advice #'other-window     'after 'eclim-problems-highlight-on-other-window)
    ;; for compiled advice
    (ad-update 'switch-to-buffer)
    (ad-update 'other-window)))

;;; packages.el ends here

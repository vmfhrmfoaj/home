;;; packages.el --- exercise layer packages file for Spacemacs.
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

(defconst exercise-packages
  '(speed-type))

(defun exercise/init-speed-type ()
  (use-package speed-type
    :config
    (setq speed-type-gb-dir (locate-user-emacs-file ".cache/speed-type"))
    (spacemacs/set-leader-keys
      "at" #'speed-type-text)))

;;; packages.el ends here

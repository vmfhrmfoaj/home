;;; packages.el --- shell-scripts-ext layer packages file for Spacemacs.
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

(defconst shell-scripts-ext-packages
  '((sh-script :location built-in)))

(defun shell-scripts-ext/post-init-sh-script ()
  (use-package sh-script
    :defer t
    :config
    (setq sh-basic-offset 2
          sh-learn-basic-offset t)))

;;; packages.el ends here

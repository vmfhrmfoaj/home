;;; packages.el --- gtags-ext layer packages file for Spacemacs.
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

(defconst gtags-ext-packages
  '(ggtags))

(defun gtags-ext/post-init-ggtags ()
  (use-package ggtags
    :no-require t
    :diminish ggtags-mode))

;;; packages.el ends here

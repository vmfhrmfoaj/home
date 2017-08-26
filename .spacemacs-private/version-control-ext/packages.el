;;; packages.el --- version-control-ext layer packages file for Spacemacs.
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

(defconst version-control-ext-packages
  '(git-gutter-fringe+))

(defun version-control-ext/post-init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :defer t
    :config
    (setq git-gutter-fr+-side 'left-fringe)
    (let* ((max     8)
           (prefix  2)
           (width   5)
           (width   (min width (- max prefix)))
           (postfix (- max prefix width))
           (bitmap  (-repeat (1- (line-pixel-height))
                             (apply #'concat (append (-repeat prefix  ".")
                                                     (-repeat width   "X")
                                                     (-repeat postfix ".")))))
           (fr-vec (apply #'fringe-helper-convert bitmap)))
      (define-fringe-bitmap 'git-gutter-fr+-added    fr-vec nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr+-deleted  fr-vec nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr+-modified fr-vec nil nil nil))))

;;; packages.el ends here

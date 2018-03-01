;;; packages.el --- tools layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `tools-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `tools/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `tools/pre-init-PACKAGE' and/or
;;   `tools/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst tools-packages
  '((atomic-chrome :location (recipe :repo "vmfhrmfoaj/atomic-chrome"
                                     :fetcher github))))

(defun tools/init-atomic-chrome ()
  (use-package atomic-chrome
    :demand t
    :config
    (atomic-chrome-start-server)))

;;; packages.el ends here

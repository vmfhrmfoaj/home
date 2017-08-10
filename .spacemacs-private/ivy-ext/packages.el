;;; packages.el --- ivy-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
;; added to `ivy-ext-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ivy-ext/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ivy-ext/pre-init-PACKAGE' and/or
;;   `ivy-ext/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ivy-ext-packages
  '(ivy))

(defun ivy-ext/post-init-ivy ()
  (use-package ivy
    :config
    (setq ivy-height 25
          ivy-fixed-height-minibuffer t)))

;;; packages.el ends here

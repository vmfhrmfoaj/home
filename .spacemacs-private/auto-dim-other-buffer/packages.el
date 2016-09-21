;;; packages.el --- auto-dim-other-buffer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: KimJinseop <Jinseop@KimJinseops-iMac.local>
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
;; added to `auto-dim-other-buffer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `auto-dim-other-buffer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `auto-dim-other-buffer/pre-init-PACKAGE' and/or
;;   `auto-dim-other-buffer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst auto-dim-other-buffer-packages
  '(
    auto-dim-other-buffers
    ))

(defun auto-dim-other-buffer/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :ensure t
    :commands adob--after-change-major-mode-hook
    :config
    (eval-after-load "diminish" '(diminish 'auto-dim-other-buffers-mode))
    (advice-add #'adob--after-change-major-mode-hook
                :override (lambda (&rest args)
                            nil))))

;;; packages.el ends here

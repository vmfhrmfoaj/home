;;; packages.el --- html-ext layer packages file for Spacemacs.
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

(defconst html-ext-packages
  '(
    css-mode
    web-mode
    ))

(defun html-ext/post-init-css-mode ()
  (use-package css-mode
    :config
    (setq-default css-indent-offset 2)))

(defun html-ext/post-init-web-mode ()
  (use-package web-mode
    :config
    (setq-default web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))

;;; packages.el ends here

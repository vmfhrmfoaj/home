;;; packages.el --- perl5-ext layer packages file for Spacemacs.
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

(defconst perl5-ext-packages
  '(company-plsense
    (cperl-mode :location built-in)))

(defun perl5-ext/post-init-company-plsense ()
  (use-package company-plsense
    :defer t
    :init
    (byte-compile #'perl-plsense-jump-to-definition)
    (add-hook 'cperl-mode-hook
              (lambda ()
                (add-to-list 'spacemacs-jump-handlers
                             #'perl-plsense-jump-to-definition))
              'append)))

(defun perl5-ext/post-init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :init
    (add-hook 'cperl-mode-hook
              (lambda ()
                (perl-setup-indent-config perl-indent-config)))))

;;; packages.el ends here

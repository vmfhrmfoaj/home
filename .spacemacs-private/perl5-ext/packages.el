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
  '((cperl-mode :location built-in)))

(defun perl5-ext/post-init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :config
    (byte-compile #'perl-set-offsets)
    (byte-compile #'perl-set-vars)
    (byte-compile #'perl-setup-indent-config)

    (add-to-list 'spacemacs-jump-handlers-cperl-mode 'dumb-jump-go)
    (let ((f (byte-compile (lambda (&rest _)))))
      (advice-add #'cperl-electric-keyword :override f)
      (advice-add #'cperl-electric-else    :override f)
      (advice-add #'cperl-electric-pod     :override f))
    (add-hook 'cperl-mode-hook
              (lambda ()
                (perl-setup-indent-config perl-indent-config)
                (setq-local beginning-of-defun-function
                            (byte-compile
                             (lambda (arg)
                               (unless (re-search-backward perl-defun-regex nil t)
                                 (error "Not found starting of the subroutine.")))))
                (setq-local end-of-defun-function
                            (lambda ()
                              (and (re-search-forward perl-defun-regex) (up-list))))))
    (font-lock-add-keywords
     'cperl-mode
     (let* ((symbol "[@$%][_0-9a-zA-Z]+")
            (whitespace "[ \r\t\n]")
            (whitespace+ (concat whitespace "+"))
            (whitespace* (concat whitespace "*")))
       `((,(concat "\\(" symbol "\\)")
          (1 (let ((face (plist-get (text-properties-at (1- (match-beginning 0))) 'face)) face-lst)
               (setq face-lst (if (listp face) face (list face)))
               (when (or (memq 'font-lock-comment-face face-lst)
                         (memq 'font-lock-string-face  face-lst))
                 face))
             t))
         (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "\\(" symbol "\\)")
          (1 'font-lock-variable-name-face))
         (,(concat "\\(?:my\\|local\\|our\\)" whitespace+ "(" )
          (,(concat "\\(" symbol "\\)")
           (save-excursion
             (safe-up-list-1)
             (point))
           nil
           (1 'font-lock-variable-name-face)))
         ("for\\(each\\)? my \\([@$%][_0-9a-zA-Z]+\\)"
          (1 'font-lock-variable-name-face))))
     'append)))

;;; packages.el ends here

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

    (let ((f (byte-compile (lambda (&rest _)))))
      (advice-add #'cperl-electric-keyword :override f)
      (advice-add #'cperl-electric-else    :override f)
      (advice-add #'cperl-electric-pod     :override f))
    (evil-define-key 'normal 'cperl-mode-map (kbd "M-,") #'pop-tag-mark)
    (add-hook 'cperl-mode-hook
              (lambda ()
                (perl-setup-indent-config perl-indent-config)
                (setq-local beginning-of-defun-function
                            (byte-compile
                             (lambda (arg)
                               (let* ((cur-pos (point))
                                      (semi-fn-regex "sub[ \r\t\n]+[_0-9A-Za-z]+\\([ \r\t\n]*([@$%]*)\\)?")
                                      (fn-regex (concat semi-fn-regex "[ \r\t\n]+{"))
                                      (from-beg-of-deufn? (eq 'beginning-of-defun this-command))
                                      (cur-line-str (buffer-substring (line-beginning-position)
                                                                      (line-end-position))))
                                 (when (or from-beg-of-deufn?
                                           (not (string-match-p semi-fn-regex cur-line-str)))
                                   (end-of-line)
                                   (unless (re-search-backward fn-regex nil t)
                                     (goto-char cur-pos)
                                     (if from-beg-of-deufn?
                                         (error "Not found starting of the subroutine.")
                                       (while (ignore-errors (backward-up-list nil t))))))))))
                (setq-local end-of-defun-function
                            (byte-compile
                             (lambda ()
                               (and (re-search-forward "{")
                                    (up-list)))))))
    (font-lock-add-keywords
     'cperl-mode
     (let* ((symbol "[@$%]+[:_0-9a-zA-Z]+")
            (whitespace "[ \r\t\n]")
            (whitespace+ (concat whitespace "+"))
            (whitespace* (concat whitespace "*")))
       `((,(concat "\\(" symbol "\\|\\(accept\\|do\\)\\s-*(\\)")
          (1 (let* ((pos (if (eolp) (1- (match-beginning 0)) (1+ (match-end 0))))
                    (face (plist-get (text-properties-at pos) 'face))
                    (face-lst (if (listp face) face (list face))))
               (when (or (memq 'font-lock-comment-face           face-lst)
                         (memq 'font-lock-comment-delimiter-face face-lst)
                         (memq 'font-lock-string-face            face-lst))
                 face))
             t))
         (,(concat "^" whitespace* "\\(sub\\)" whitespace+ "\\([_0-9A-Za-z]+\\)\\(?:" whitespace* "([@$%]*)\\)?")
          (1 'font-lock-keyword-face)
          (2 'font-lock-function-name-face t))
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
          (1 'font-lock-variable-name-face))
         (,(concat whitespace "\\(accept\\)" whitespace* "(")
          (1 'font-lock-type-face))))
     'append)))

;;; packages.el ends here

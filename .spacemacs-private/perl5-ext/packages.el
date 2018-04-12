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
  '((cperl-mode :location built-in)
    (perl5db-as-repl :location (recipe :repo "vmfhrmfoaj/perl5db-as-repl" :fetcher gitlab))))

(defun perl5-ext/post-init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :config
    (byte-compile #'perl-set-offsets)
    (byte-compile #'perl-set-vars)
    (byte-compile #'perl-setup-indent-config)

    (setq cperl-break-one-line-blocks-when-indent nil
          cperl-fix-hanging-brace-when-indent nil
          cperl-indent-region-fix-constructs nil
          cperl-indent-wrt-brace nil
          cperl-merge-trailing-else nil)

    (spacemacs/declare-prefix-for-mode 'cperl-mode "ms" "repl")
    (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
      "sc" 'perl5db-as-repl/connect-repl
      "ss" 'perl5db-as-repl/switch-to-repl)

    (let ((f (byte-compile (lambda (&rest _) "Do nothing" nil))))
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
          (1 (cond
              ((sp-point-in-string)  'font-lock-string-face)
              ((sp-point-in-comment) 'font-lock-comment-face)
              (t nil))
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

(defun perl5-ext/init-perl5db-as-repl ()
  (use-package perl5db-as-repl
    :init
    (defun perl5db-as-repl/connect-repl ()
      (interactive)
      (with-current-buffer (get-buffer-create "*repl5db-as-repl*")
        (when (boundp 'perl5db-as-repl/port)
          (perl5db-as-repl//server-stop perl5db-as-repl/port))
        (perl5db-as-repl-mode)))

    (defun perl5db-as-repl/switch-to-repl ()
      (interactive)
      (-if-let (buf (get-buffer "*repl5db-as-repl*"))
          (switch-to-buffer buf)
        (error "There is no REPL connection")))))

;;; packages.el ends here

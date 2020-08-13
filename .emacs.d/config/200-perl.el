;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load-file "~/.emacs.d/func.el"))

(use-package cperl-mode
  :defer t
  :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
  :init
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl"  . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  :config
  (defun cperl-beginning-of-defun ()
    "TODO"
    (let ((cur-pos (point))
          (beg-pos (line-beginning-position))
          (end-pos (line-end-position))
          (regex "^\\s-*sub\\s-*[_0-9A-Za-z]+\\(?:\\s-*([ $@%]+)\\)?")
          (n 0))
      (if (and (string-match-p regex (buffer-substring beg-pos end-pos))
               (not (= beg-pos cur-pos)))
          (beginning-of-line)
        (unless (re-search-backward (concat regex "[ \t\r\n]*{") nil t)
          (goto-char cur-pos)
          (condition-case nil
              (while (backward-up-list) (setq n (1+ n)))
            ;; the indentation function of `cperl-mode' seems to used `beginning-of-line' function.
            (error
             (and (zerop n)
                  (message "Not found starting of the subroutine."))))))))

  (defun cperl-end-of-defun ()
    "TODO"
    (let ((cur-pos (point))
          (beg-pos (line-beginning-position))
          (end-pos (line-end-position))
          (regex "^\\s-*sub\\s-*[_0-9A-Za-z]+\\(?:\\s-*([ $@%]+)\\)?"))
      (if (string-match-p regex (buffer-substring beg-pos end-pos))
          (progn
            (beginning-of-line)
            (and (re-search-forward "[ \t\r\n]*{")
                 (up-list)))
        (re-search-forward regex nil t))))

  (defun cperl-mode-setup ()
    "TODO"
    (setq-local beginning-of-defun-function #'cperl-beginning-of-defun)
    (setq-local end-of-defun-function       #'cperl-end-of-defun))

  (add-hook 'cperl-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point))))

  (modify-syntax-entry ?: "." cperl-mode-syntax-table)
  (setq cperl-break-one-line-blocks-when-indent nil
        cperl-fix-hanging-brace-when-indent nil
        cperl-indent-parens-as-block t
        cperl-indent-region-fix-constructs nil
        cperl-indent-wrt-brace nil
        cperl-merge-trailing-else nil)
  (with-eval-after-load "smartparens"
    (add-to-list 'sp-sexp-prefix '(cperl-mode regexp "\\(?:qw\\)")))
  (let ((f (byte-compile (-const nil))))
    (advice-add #'cperl-electric-keyword :override f)
    (advice-add #'cperl-electric-else    :override f)
    (advice-add #'cperl-electric-pod     :override f))
  (add-hook 'cperl-mode-hook #'cperl-mode-setup))

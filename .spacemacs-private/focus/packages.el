;;; packages.el --- focus layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst focus-packages
  '(evil
    focus))

(defun focus/post-init-evil ()
  (add-hook 'evil-insert-state-entry-hook (lambda () (focus-mode 1)))
  (add-hook 'evil-insert-state-exit-hook  (lambda () (focus-mode 0))))

(defun focus/init-focus ()
  (use-package focus
    :ensure t
    :config
    (add-to-list 'focus-mode-to-thing '(tex-mode . tex-sentence))
    (add-to-list 'focus-mode-to-thing '(org-mode . org))
    (setq focus-mode-to-thing (append focus-mode-to-new-thing focus-mode-to-thing))
    (put 'org 'bounds-of-thing-at-point
         (lambda ()
           (save-excursion
             (let ((start (progn
                            (outline-previous-heading)
                            (point)))
                   (end   (progn
                            (outline-next-visible-heading 1)
                            (beginning-of-line)
                            (point))))
               (cons start end)))))
    (put 'tex-sentence 'bounds-of-thing-at-point
         (lambda ()
           (let* ((regx  (concat "^\\(?:[[:cntrl:]]\\)*$"))
                  (start (save-excursion
                           (backward-char)
                           (re-search-backward regx nil t)
                           (point)))
                  (end   (save-excursion
                           (forward-char)
                           (re-search-forward regx nil t)
                           (point))))
             (cons start end))))
    (put 'list+ 'bounds-of-thing-at-point (get 'list 'bounds-of-thing-at-point))
    (eval-after-load "smartparens"
      '(put 'list+ 'bounds-of-thing-at-point
            (lambda ()
              (save-excursion
                (let ((start (progn
                               (ignore-errors
                                 (cond ((sp-point-in-string)
                                        (save-match-data
                                          (re-search-backward "[^\\]\""))
                                        (forward-char))
                                       ((sp-point-in-comment)
                                        (beginning-of-line)))
                                 (backward-up-list 2))
                               (point)))
                      (end (progn
                             (forward-list)
                             (point))))
                  (cons start end))))))))

;;; packages.el ends here

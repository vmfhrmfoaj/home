;;; packages.el --- spacemacs-evil-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst spacemacs-evil-ext-packages
  '(evil-surround
    linum-relative))

(defun spacemacs-evil-ext/post-init-evil-surround ()
  (use-package evil-surround
    :defer t
    :config
    (advice-add #'evil-surround-region :filter-args
                (lambda (args)
                  (if (> 4 (length args))
                      args
                    (let* ((char     (nth 3 args))
                           (new-char (cond
                                      ((= char 33554477) 95)
                                      ((= char 33554475) 61)
                                      ((= char 33554479) 92))))
                      (if new-char
                          (-replace-at 3 new-char args)
                        args)))))))

(defun spacemacs-evil-ext/post-init-linum-relative ()
  (use-package linum-relative
    :config
    (linum-relative-global-mode)
    (add-hook 'linum-relative-mode-hook (-partial #'diminish 'linum-relative-mode))
    (add-hook 'linum-mode-hook
              (lambda ()
                (setq-local linum-relative-format
                            (concat "%"
                                    (-> (count-lines (point-min) (point-max))
                                        (number-to-string)
                                        (length)
                                        (min 5)
                                        (max 3)
                                        (number-to-string))
                                    "s"))))))

;;; packages.el ends here

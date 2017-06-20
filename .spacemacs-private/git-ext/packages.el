;;; packages.el --- git-ext layer packages file for Spacemacs.
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

(defconst git-ext-packages
  '(magit))

(defun git-ext/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (setq magit-diff-refine-hunk t)
    (add-hook 'magit-revision-mode-hook (lambda () (setq-local line-spacing 0))))

  (use-package magit-blame
    :defer
    :config
    (define-key magit-blame-mode-map "n" nil)
    (define-key magit-blame-mode-map "N" nil)))

;;; packages.el ends here

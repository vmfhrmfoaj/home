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
  '(
    magit
    ;; with-editor
    ))

(defun git-ext/post-init-magit ()
  (use-package magit
    :config
    (setq magit-diff-refine-hunk t)
    (add-to-list 'magit-diff-section-arguments "--compaction-heuristic")
    (add-hook 'magit-revision-mode-hook
              (lambda ()
                (setq-local line-spacing 0)))))

;; NOTE
;;  This setting need for the `railwaycat/emacsmacport'.
;; (defun with-editor/post-init-with-editor ()
;;   (use-package with-editor
;;     :config
;;     (setq with-editor-emacsclient-executable
;;           "/usr/local/opt/emacs-mac/bin/emacsclient")))

;;; packages.el ends here

;;; packages.el --- eye-candy layer packages file for Spacemacs. ;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `eye-candy-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `eye-candy/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `eye-candy/pre-init-PACKAGE' and/or
;;   `eye-candy/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst eye-candy-packages
  '(
    all-the-icons
    neotree
    ))

(defun eye-candy/init-all-the-icons ()
  (use-package all-the-icons
    :config
    (setq all-the-icons-default-adjust 0.0
          all-the-icons-scale-factor 0.9)
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.edn$"
                   all-the-icons-alltheicon "clojure"
                   :height 1.0
                   :face all-the-icons-blue))

    ;; NOTE
    ;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line#modified-or-read-only
    (eval-after-load "spaceline-segments"
      '(progn
         (spaceline-define-segment buffer-modified
           (let* ((name (cond ((not buffer-file-name) "times-circle")
                              ((buffer-modified-p)    "plus-circle")
                              (buffer-read-only       "minus-circle")
                              (t                      "check-circle")))
                  (icon (all-the-icons-faicon name :v-adjust 0.0)))
             (powerline-raw (propertize icon 'face nil)
                            `(:family ,(all-the-icons-faicon-family)))))

         ;; (spaceline-define-segment major-mode
         ;;   (let* ((file-name (or buffer-file-name "foo"))
         ;;          (font-family (all-the-icons-icon-family-for-file file-name))
         ;;          (mode-symbol (all-the-icons-icon-for-file file-name :v-adjust -0.05))
         ;;          (not-found? (string-equal mode-symbol (all-the-icons-faicon "file-o"))))
         ;;     (flet ((format-mode-line
         ;;             (&rest _)
         ;;             (if not-found?
         ;;                 mode-name
         ;;               (propertize mode-symbol 'face nil))))
         ;;       (powerline-major-mode (unless not-found? `(:family ,font-family))))))
         ))))

(defun eye-candy/post-init-neotree ()
  (use-package neotree
    :config
    (setq neo-theme 'icons)))

;;; packages.el ends here

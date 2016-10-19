;; ; packages.el --- eye-candy layer packages file for Spacemacs.
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

(defconst eye-candy-packages
  '(all-the-icons
    auto-dim-other-buffers
    neotree
    (prettify-symbols-mode :location built-in)))

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

(defun eye-candy/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :ensure t
    :config
    (eval-after-load "diminish" '(diminish 'auto-dim-other-buffers-mode))
    (advice-add #'adob--after-change-major-mode-hook :override
                (lambda (&rest args)
                  nil))
    (auto-dim-other-buffers-mode)))

(defun eye-candy/post-init-neotree ()
  (use-package neotree
    :config
    (setq neo-theme 'icons)))

(defun eye-candy/init-prettify-symbols-mode ()
  (use-package prog-mode
    :commands (global-prettify-symbols-mode)
    :config
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
    (add-hook 'prog-mode-hook
              (-partial #'font-lock-add-keywords nil
                        fira-code-font-lock-keywords-alist))
    (global-prettify-symbols-mode)

    ;; Change the behavior of indent function for `prettify-symbols-mode'.
    (advice-disable-modes '(prettify-symbols-mode) #'indent-for-tab-command)
    (advice-disable-modes '(prettify-symbols-mode) #'indent-region)
    (advice-disable-modes '(prettify-symbols-mode) #'indent-according-to-mode)))

;;; packages.el ends here

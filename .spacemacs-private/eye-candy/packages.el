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
    company
    evil
    golden-ratio
    neotree
    (prettify-symbols-mode :location built-in)))

(defun eye-candy/init-all-the-icons ()
  (use-package all-the-icons
    :after spaceline-segments
    :config
    (setq all-the-icons-default-adjust 0.0
          all-the-icons-scale-factor 0.9)
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.edn$"
                   all-the-icons-alltheicon "clojure"
                   :height 1.0
                   :face all-the-icons-blue))
    (spaceline-define-segment buffer-modified
      (let* ((name (cond ((not buffer-file-name) "times-circle")
                         ((buffer-modified-p)    "plus-circle")
                         (buffer-read-only       "minus-circle")
                         (t                      "check-circle")))
             (icon (all-the-icons-faicon name :v-adjust -0.1)))
        (powerline-raw (propertize icon 'face nil)
                       `(:family ,(all-the-icons-faicon-family)))))
    (spaceline-define-segment major-mode
      (let* ((file-name (or buffer-file-name "foo"))
             (font-family (all-the-icons-icon-family-for-file file-name))
             (mode-symbol (all-the-icons-icon-for-file file-name))
             (not-found? (string-equal mode-symbol (all-the-icons-faicon "file-o"))))
        (flet ((format-mode-line
                (&rest _)
                (if not-found?
                    mode-name
                  (propertize mode-symbol 'face nil))))
          (powerline-major-mode (unless not-found? `(:family ,font-family))))))
    (spaceline-define-segment version-control
      (when vc-mode
        (powerline-raw (concat (all-the-icons-octicon "git-branch" :height 1.2)
                               " " (cadr (split-string vc-mode "[-:]"))))))))

(defun eye-candy/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :after dash
    :ensure t
    :config
    (with-eval-after-load 'diminish
      (diminish 'auto-dim-other-buffers-mode))
    (advice-add #'adob--after-change-major-mode-hook :override
                (lambda (&rest args)
                  nil))
    (auto-dim-other-buffers-mode)))

(defun eye-candy/post-init-company ()
  (use-package company
    :defer t
    :config
    (setq company-tooltip-exclude-modes '(prettify-symbols-mode page-break-lines-mode)
          company-tooltip-exclude-mode-status nil)
    (advice-add #'company-call-frontends :before
                (lambda (cmd)
                  (cond
                   ((eq 'show cmd)
                    (setq-local company-tooltip-exclude-mode-status
                                (-map #'symbol-value company-tooltip-exclude-modes))
                    (disable-modes company-tooltip-exclude-modes)
                    (redisplay))
                   ((eq 'hide cmd)
                    (restore-modes company-tooltip-exclude-modes
                                   company-tooltip-exclude-mode-status)))))))

(defun eye-candy/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (advice-add #'evil-next-line :around
                (lambda (of &rest args)
                  (let* ((str (save-excursion
                                (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position 2))))
                         (modes (if (string-match-p "->\\|.-\\|;;" str)
                                    '(prettify-symbols-mode)
                                  nil)))
                    (with-disable-modes modes
                      (apply of args)))))
    (advice-add #'evil-previous-line :around
                (lambda (of &rest args)
                  (let* ((str (save-excursion
                                (buffer-substring-no-properties (line-beginning-position 0)
                                                                (line-end-position))))
                         (modes (if (string-match-p "->\\|.-\\|;;" str)
                                    '(prettify-symbols-mode)
                                  nil)))
                    (with-disable-modes modes
                      (apply of args)))))
    (setq evil-visual-state-exclude-modes '(prettify-symbols-mode))
    (add-hook 'evil-visual-state-entry-hook
              (lambda ()
                (setq-local evil-visual-state-exclude-mode-status
                            (-map #'symbol-value evil-visual-state-exclude-modes))
                (disable-modes evil-visual-state-exclude-modes)))
    (add-hook 'evil-visual-state-exit-hook
              (lambda ()
                (restore-modes evil-visual-state-exclude-modes
                               evil-visual-state-exclude-mode-status)))))

(defun eye-candy/post-init-golden-ratio ()
  (use-package golden-ratio
    :config
    (setq golden-ratio-adjust-factor 0)
    (setq-default truncate-lines t)))

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
    (advice-disable-modes '(prettify-symbols-mode) #'indent-for-tab-command)
    (advice-disable-modes '(prettify-symbols-mode) #'indent-region)
    (advice-disable-modes '(prettify-symbols-mode) #'indent-according-to-mode)
    (global-prettify-symbols-mode)))

;;; packages.el ends here

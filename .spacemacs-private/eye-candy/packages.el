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
    git-gutter-fringe+
    neotree
    (prettify-symbols-mode :location built-in)
    rainbow-delimiters))

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
             (icon (all-the-icons-faicon name :v-adjust 0.0)))
        (powerline-raw (propertize icon 'face nil)
                       `(:family ,(all-the-icons-faicon-family)))))))

(defun eye-candy/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :after dash
    :ensure t
    :config
    (cond ((eq 'spacemacs-dark (car dotspacemacs-themes))
           (set-face-attribute 'auto-dim-other-buffers-face
                               nil
                               :foreground
                               (dim-color (face-attribute 'default :foreground) 5))
           (set-face-attribute 'auto-dim-other-buffers-face
                               nil
                               :background
                               (dim-color (face-attribute 'default :background) 2))))
    (eval-after-load "diminish" '(diminish 'auto-dim-other-buffers-mode))
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
                    (disable-modes company-tooltip-exclude-modes))
                   ((eq 'hide cmd)
                    (restore-modes company-tooltip-exclude-modes
                                   company-tooltip-exclude-mode-status)))))))

(defun seye-candy/post-init-evil ()
  (when (require 'evil nil 'noerr)
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

(defun eye-candy/post-init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :defer t
    :config
    (let ((added    (face-attribute 'git-gutter+-added    :foreground))
          (modified (face-attribute 'git-gutter+-modified :foreground))
          (deleted  (face-attribute 'git-gutter+-deleted  :foreground)))
      (set-face-attribute 'git-gutter-fr+-added nil :foreground (dim-color added 30))
      (set-face-attribute 'git-gutter-fr+-modified nil :foreground (dim-color modified 25))
      (set-face-attribute 'git-gutter-fr+-deleted nil :foreground (dim-color deleted 20)))))

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

(defun eye-candy/post-init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :config
    (cond ((eq 'spacemacs-dark (car dotspacemacs-themes))
           (dolist (i (number-sequence 1 9))
             (let ((face (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))))
               (set-face-attribute face nil :foreground
                                   (dim-color (face-attribute face :foreground) 10))))))))

;;; packages.el ends here

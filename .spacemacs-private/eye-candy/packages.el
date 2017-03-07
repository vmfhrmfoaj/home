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
             (icon (all-the-icons-faicon name :v-adjust 0.0)))
        (powerline-raw (propertize icon 'face nil)
                       `(:family ,(all-the-icons-faicon-family)))))
    (spaceline-define-segment major-mode
      (let* ((font-family (all-the-icons-icon-family-for-buffer))
             (symbol (all-the-icons-icon-for-buffer))
             (height (cond
                      ((eq 'emacs-lisp-mode major-mode) 0.95)
                      (t 1.0)))
             (offset (cond
                      ((string-match-p "^README" (buffer-name)) -0.05)
                      ((eq 'emacs-lisp-mode major-mode) -0.1)
                      ((eq 'swift-mode major-mode) -0.2)
                      ((eq 'lisp-interaction-mode major-mode) 0.0)
                      ((eq 'magit-status-mode major-mode) -0.1)
                      ((eq 'org-mode major-mode) 0.1)
                      ((derived-mode-p 'clojure-mode) -0.1)
                      (t -0.05)))
             (new-raise (ignore-errors
                          (with-temp-buffer
                            (insert symbol)
                            (-> (text-properties-at 1)
                                (plist-get 'display)
                                (plist-get 'raise)
                                (- offset)))))
             (not-found? (not font-family)))
        (flet ((format-mode-line
                (&rest _)
                (if not-found?
                    mode-name
                  (propertize symbol 'display `(raise ,new-raise) 'face nil))))
          (powerline-major-mode (unless not-found? `(:family ,font-family :height ,height))))))
    (spaceline-define-segment version-control
      (-when-let (branch (-some-> vc-mode
                                  (split-string "[-:@]")
                                  (rest)
                                  (-some->> (-interpose "-")
                                            (apply #'concat))))
        (powerline-raw (-> "git-branch"
                           (all-the-icons-octicon :v-adjust 0.05)
                           (propertize 'face nil)
                           (concat " " branch)))))))

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
    (setq evil-replace-state-cursor '("chocolate" (hbar . 4)))
    (eval-after-load "dash"
      '(defvar fira-code-font-lock-keywords-regx
         (->> fira-code-font-lock-keywords-alist
              (-map #'first)
              (-interpose "\\|")
              (apply #'concat))))
    (let ((byte-compile-warnings nil)
          (byte-compile-dynamic t))
      (advice-add #'evil-next-line :around
                  (byte-compile
                   (lambda (of &rest args)
                     (let ((ok? (save-mark-and-excursion
                                 (let ((point (line-beginning-position)))
                                   (forward-line 1)
                                   (end-of-line)
                                   (re-search-backward fira-code-font-lock-keywords-regx
                                                       point t 1)))))
                       (with-disable-modes (when ok?
                                             '(prettify-symbols-mode))
                         (apply of args))))))
      (advice-add #'evil-previous-line :around
                  (byte-compile
                   (lambda (of &rest args)
                     (let ((ok? (save-mark-and-excursion
                                 (let ((point (point)))
                                   (forward-line -1)
                                   (beginning-of-line)
                                   (re-search-forward fira-code-font-lock-keywords-regx
                                                      point t 1)))))
                       (with-disable-modes (when ok?
                                             '(prettify-symbols-mode))
                         (apply of args))))))
      (setq-default exclude-mode-status '(t))
      (add-hook 'evil-visual-state-entry-hook
                (byte-compile
                 (lambda ()
                   (setq-local exclude-mode-status
                               (-map #'symbol-value '(prettify-symbols-mode)))
                   (disable-modes '(prettify-symbols-mode)))))
      (add-hook 'evil-insert-state-exit-hook
                (byte-compile
                 (lambda ()
                   (when evil-insert-vcount
                     (setq-local exclude-mode-status
                                 (-map #'symbol-value '(prettify-symbols-mode)))
                     (disable-modes '(prettify-symbols-mode))))))
      (add-hook 'evil-normal-state-entry-hook
                (byte-compile
                 (lambda ()
                   (restore-modes '(prettify-symbols-mode) exclude-mode-status))))
      (add-hook 'evil-visual-state-exit-hook
                (byte-compile
                 (lambda ()
                   (restore-modes '(prettify-symbols-mode) exclude-mode-status)))))))

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

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
    clojure-mode
    company
    evil
    golden-ratio
    neotree
    (prettify-symbols-mode :location built-in)))

(defun eye-candy/init-all-the-icons ()
  ;; see, https://github.com/jwiegley/use-package/issues/440
  (use-package all-the-icons
    :config
    (with-eval-after-load 'spaceline-segments
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
                        ((eq 'clojurescript-mode major-mode) 0.1)
                        ((derived-mode-p 'clojure-mode) -0.1)
                        ((eq 'emacs-lisp-mode major-mode) -0.1)
                        ((eq 'lisp-interaction-mode major-mode) 0.0)
                        ((eq 'magit-status-mode major-mode) -0.1)
                        ((eq 'makefile-bsdmake-mode major-mode) 0.1)
                        ((eq 'org-mode major-mode) 0.1)
                        ((eq 'swift-mode major-mode) -0.2)
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
                             (concat " " branch))))))))

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

(defun eye-candy/post-init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (advice-add #'clojure-font-lock-extend-region-def :around
                (lambda (of &rest args)
                  (if without-composition-prop
                      nil
                    (apply of args))))))

(defun eye-candy/post-init-company ()
  (use-package company
    :defer t
    :config
    (let ((byte-compile-warnings nil)
          (byte-compile-dynamic  t)
          (f (lambda (cmd)
               (ignore-errors
                 (cond
                  ((eq 'post-command cmd)
                   (with-silent-modifications
                     (remove-text-properties (point-min) (point-max) '(composition nil))))
                  ((eq 'hide cmd)
                   (font-lock-flush)))))))
      (advice-add #'company-pseudo-tooltip-frontend
                  :before (byte-compile f)))))

(defun eye-candy/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (setq composition-props nil
          evil-replace-state-cursor '("chocolate" (hbar . 4)))
    (let ((byte-compile-warnings nil)
          (byte-compile-dynamic  t))
      (advice-add #'evil-insert :before
                  (byte-compile
                   (lambda (&rest _)
                     (while (and (get-text-property (point)      'composition)
                                 (get-text-property (1- (point)) 'composition))
                       (backward-char)))))
      (advice-add #'evil-append :before
                  (byte-compile
                   (lambda (&rest _)
                     (while (and (get-text-property (point) 'composition)
                                 (get-text-property (1+ (point)) 'composition))
                       (forward-char)))))
      (add-hook 'evil-insert-state-exit-hook
                (byte-compile
                 (lambda ()
                   (ignore-errors
                     (while (and (get-text-property (- (point) 1) 'composition)
                                 (get-text-property (- (point) 2) 'composition))
                       (backward-char))))))
      (advice-add #'current-column :around
                  (byte-compile
                   (lambda (of &rest args)
                     (let ((beg (line-beginning-position))
                           (end (line-end-position)))
                       (without-text-property beg end 'composition
                         (apply of args))))))
      (dolist (f '(evil-next-line evil-next-visual-line))
        (advice-add f :around
                    (byte-compile
                     (lambda (of &rest args)
                       (let ((beg (line-beginning-position))
                             (end (line-end-position 2)))
                         (without-text-property beg end 'composition
                           (apply of args)))))))
      (dolist (f '(evil-previous-line evil-previous-visual-line))
        (advice-add f :around
                    (byte-compile
                     (lambda (of &rest args)
                       (let ((beg (line-beginning-position -1))
                             (end (line-end-position 2)))
                         (without-text-property beg end 'composition
                           (apply of args)))))))
      (advice-add #'evil-visual-highlight-block :around
                  (byte-compile
                   (lambda (of beg end &optional overlays)
                     (let ((beg_ (save-excursion
                                   (goto-char beg)
                                   (line-beginning-position)))
                           (end_ (save-excursion
                                   (goto-char end)
                                   (line-end-position))))
                       (without-text-property beg_ end_ 'composition
                         (funcall of beg end overlays))))))
      (advice-add #'evil-delete :around
                  (lambda (of &rest args)
                    (if (evil-visual-state-p)
                        (without-text-property-hard (point-min) (point-max) 'composition
                          (apply of args))
                      (apply of args))))
      (setq-default without-composition-prop nil)
      (advice-add #'evil-insert :before
                  (byte-compile
                   (lambda (&rest _)
                     (when (and (not without-composition-prop)
                                (evil-visual-state-p))
                       (setq-local without-composition-prop t)
                       (with-silent-modifications
                         (remove-text-properties (point-min)
                                                 (point-max)
                                                 (list 'composition nil)))))))
      (add-hook 'evil-normal-state-entry-hook
                (byte-compile
                 (lambda ()
                   (when without-composition-prop
                     (setq-local without-composition-prop nil)
                     (font-lock-flush))))))))

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
    (dolist (f '(indent-for-tab-command indent-region indent-according-to-mode))
      (advice-add f :around
                  (byte-compile
                   (lambda (of &rest args)
                     (without-text-property-hard (point-min) (point-max) 'composition
                       (apply of args))))))
    (global-prettify-symbols-mode)))

;;; packages.el ends here

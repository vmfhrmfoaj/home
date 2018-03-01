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
    evil-goggles
    fancy-narrow
    golden-ratio
    neotree))

(defun eye-candy/init-all-the-icons ()
  (use-package all-the-icons
    :after spaceline-segments
    :demand t
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
                      ((string-match-p "\\<test\\>" (or buffer-file-name "")) 0.15)
                      ((eq 'clojurescript-mode major-mode) 0.1)
                      ((derived-mode-p 'clojure-mode) -0.1)
                      ((eq 'emacs-lisp-mode major-mode) -0.1)
                      ((eq 'lisp-interaction-mode major-mode) 0.05)
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
                           (concat " " branch)))))))

(defun eye-candy/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :disabled t ; screen flickering...
    :demand t
    :after dash
    :config
    (with-eval-after-load 'diminish
      (diminish 'auto-dim-other-buffers-mode))
    (with-eval-after-load 'helm
      (add-hook 'helm-after-action-hook
                (byte-compile
                 (lambda ()
                   (adob--dim-all-buffers t)
                   (adob--dim-buffer nil)
                   (setq adob--last-buffer (current-buffer)))))
      (advice-add #'adob--ignore-buffer :filter-return
                  (byte-compile
                   (lambda (ret)
                     (or ret (helm-alive-p))))))
    (auto-dim-other-buffers-mode)))

(defun eye-candy/post-init-company ()
  (unless (string-equal "Fira Code" (car dotspacemacs-default-font))
    (use-package company
      :defer t
      :config
      (let ((f (lambda (cmd)
                 (ignore-errors
                   (cond
                    ((eq 'post-command cmd)
                     (with-silent-modifications
                       (remove-text-properties (point-min) (point-max) '(composition nil))))
                    ((eq 'hide cmd)
                     (font-lock-flush)))))))
        (advice-add #'company-pseudo-tooltip-frontend
                    :before (byte-compile f))))))

(defun eye-candy/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (setq composition-props nil
          evil-replace-state-cursor '("chocolate" (hbar . 4)))

    (unless (string-equal "Fira Code" (car dotspacemacs-default-font))
      (with-eval-after-load "dash"
        (byte-compile #'get-text-properties)
        (byte-compile #'put-text-properties)
        (byte-compile #'without-text-property)
        (byte-compile #'without-text-property-hard))

      (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
      (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
      (add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords nil fira-code-font-lock-keywords-alist 'append)))
      (dolist (f '(indent-for-tab-command indent-region indent-according-to-mode))
        (advice-add f :around
                    (byte-compile
                     (lambda (of &rest args)
                       (without-text-property-hard (point-min) (point-max) 'composition
                         (apply of args))))))

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
                    (if (and (evil-visual-state-p)
                             (eq 'block (evil-visual-type)))
                        (without-text-property-hard (point-min) (point-max) 'composition
                          (apply of args))
                      (apply of args))))
      (advice-add #'evil-replace :around
                  (lambda (of &rest args)
                    (if (and (evil-visual-state-p)
                             (eq 'block (evil-visual-type)))
                        (let ((beg (save-excursion
                                     (goto-char evil-visual-beginning)
                                     (line-beginning-position)))
                              (end (save-excursion
                                     (goto-char evil-visual-end)
                                     (line-end-position))))
                          (without-text-property-hard beg end 'composition
                            (apply of args)))
                      (apply of args))))
      (setq without-composition-prop nil)
      (let ((disable-comp-prop (byte-compile
                                (lambda ()
                                  (when (not without-composition-prop)
                                    (setq-local without-composition-prop t)
                                    (with-silent-modifications
                                      (remove-text-properties (point-min)
                                                              (point-max)
                                                              (list 'composition nil)))))))
            (restore-comp-porp (byte-compile
                                (lambda ()
                                  (when without-composition-prop
                                    (setq-local without-composition-prop nil)
                                    (font-lock-flush))))))
        (add-hook 'evil-visual-state-entry-hook disable-comp-prop)
        (add-hook 'evil-visual-state-exit-hook  restore-comp-porp))
      (advice-add #'evil-cleanup-insert-state :around
                  (byte-compile
                   (lambda (of)
                     (if evil-insert-vcount
                         (without-text-properties-hard (point-min) (point-max) '(composition font-lock-multiline)
                           (funcall of))
                       (funcall of))))))))

(defun eye-candy/init-evil-goggles ()
  (use-package evil-goggles
    :demand t
    :config
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces)))

(defun eye-candy/init-fancy-narrow ()
  (use-package fancy-narrow
    :demand t
    :config
    (advice-add #'fancy-narrow-to-region :after
                (lambda (&rest _)
                  (setq-local helm-swoop-list-cache nil)))
    (advice-add #'fancy-widen :after
                (lambda (&rest _)
                  (kill-local-variable 'helm-swoop-list-cache)))
    (advice-add #'save-buffer :around
                (lambda (fn &optional arg)
                  (let (fancy-narrow--beginning fancy-narrow--end)
                    (funcall fn arg))))
    (advice-add #'jit-lock-function :around
                (byte-compile
                 (lambda (fn start)
                   (let (fancy-narrow--beginning fancy-narrow--end)
                     (funcall fn start)))))))

(defun eye-candy/post-init-golden-ratio ()
  (use-package golden-ratio
    :demand t
    :config
    (setq golden-ratio-adjust-factor 0)
    (setq-default truncate-lines t)))

(defun eye-candy/post-init-neotree ()
  (use-package neotree
    :defer t
    :init
    (setq neo-theme 'icons)))

;;; packages.el ends here

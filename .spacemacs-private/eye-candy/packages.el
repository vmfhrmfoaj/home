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

(defun eye-candy/post-init-company ()
  (use-package company
    :defer t
    :config
    (let ((byte-compile-warnings nil)
          (byte-compile-dynamic  t)
          (f (lambda (cmd)
               (ignore-errors
                 (cond
                  ((eq 'show cmd)
                   (let ((start (window-start))
                         (end   (window-end)))
                     (setq-local company-tt-start-pos (point))
                     (setq-local company-composition-props (get-text-properties start end 'composition))
                     (with-silent-modifications
                       (remove-text-properties start end '(composition nil)))))
                  ((eq 'hide cmd)
                   (with-silent-modifications
                     (let ((delta (- (point) company-tt-start-pos)))
                       (->> company-composition-props
                            (--map (let* ((pos (car it))
                                          (pos (if (> company-tt-start-pos pos)
                                                   pos
                                                 (+ delta pos))))
                                     (cons pos (cdr it))))
                            (put-text-properties))
                       (setq-local company-composition-props nil)))))))))
      (advice-add #'company-call-frontends :before
                  (byte-compile f))
      )))

(defun eye-candy/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (setq composition-props nil
          evil-replace-state-cursor '("chocolate" (hbar . 4)))
    (let ((byte-compile-warnings nil)
          (byte-compile-dynamic  t))
      (advice-add #'evil-next-line :around
                  (byte-compile
                   (lambda (of &rest args)
                     (let ((start (line-beginning-position))
                           (end   (line-end-position 2)))
                       (without-text-property start end 'composition
                         (apply of args))))))
      (advice-add #'evil-previous-line :around
                  (byte-compile
                   (lambda (of &rest args)
                     (let ((start (line-beginning-position -1)))
                       (without-text-property start (point) 'composition
                         (apply of args))))))
      (add-hook 'evil-visual-state-entry-hook
                (byte-compile
                 (lambda ()
                   (let ((start (window-start))
                         (end   (window-end)))
                     (setq-local composition-props (get-text-properties start end 'composition))
                     (with-silent-modifications
                       (remove-text-properties start end '(composition nil)))))))
      (add-hook 'evil-visual-state-exit-hook
                (byte-compile
                 (lambda ()
                   (with-silent-modifications
                     (put-text-properties composition-props))
                   (setq-local composition-props nil))))
      (add-hook 'evil-insert-state-exit-hook
                (byte-compile
                 (lambda ()
                   (when evil-insert-vcount
                     (let ((start (window-start))
                           (end   (window-end)))
                       (setq-local composition-props (get-text-properties start end 'composition))
                       (with-silent-modifications
                         (remove-text-properties start end '(composition nil))))))))
      (add-hook 'evil-normal-state-entry-hook
                (byte-compile
                 (lambda ()
                   (with-silent-modifications
                     (put-text-properties composition-props))
                   (setq-local composition-props nil)))))))

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

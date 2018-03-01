;;; packages.el --- spacemacs-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
;; added to `spacemacs-ext-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacemacs-ext/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacemacs-ext/pre-init-PACKAGE' and/or
;;   `spacemacs-ext/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-ext-packages
  '((ediff :location built-in)
    auto-highlight-symbol
    evil
    evil-surround
    hl-todo
    (linum :location built-in)
    linum-relative
    persp-mode
    projectile
    rainbow-delimiters
    smartparens
    spaceline
    which-key
    (whitespace :location built-in)))

(defun spacemacs-ext/post-init-ediff ()
  (use-package ediff
    :defer t
    :config
    ;; NOTE
    ;; prevent to calculate the width of the window
    ;;  in `ediff-setup-windows-plain-compare' function.
    (setq ediff-exclude-modes '(golden-ratio-mode)
          ediff-split-window-function (lambda (&rest _) (split-window-right)))
    (advice-add #'ediff-setup :before
                (lambda (&rest _)
                  (setq ediff-exclude-mode-status (-map #'symbol-value ediff-exclude-modes))
                  (disable-modes ediff-exclude-modes)
                  (unless (cdr (assoc 'fullscreen (frame-parameters)))
                    (spacemacs/toggle-maximize-frame-on))))
    (advice-add #'ediff-quit :after
                (lambda (&rest _)
                  (restore-modes ediff-exclude-modes ediff-exclude-mode-status)
                  (when (eq 'maximized (cdr (assoc 'fullscreen (frame-parameters))))
                    (spacemacs/toggle-maximize-frame-off))))))

(defun spacemacs-ext/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :config
    (setq ahs-include '((clojure-mode . "[^ \r\t\n]+")
                        (clojurescript-mode . "[^ \r\t\n]+")
                        (clojurec-mode . "[^ \r\t\n]+")
                        (emacs-lisp-mode . "[^ \r\t\n]+")))))

(defun spacemacs-ext/post-init-evil ()
  (when (require 'evil nil 'noerr)
    (define-key evil-ex-map (kbd "C-h") #'delete-backward-char)
    (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
    (add-hook 'evil-normal-state-entry-hook #'auto-indent)
    (add-hook 'evil-insert-state-entry-hook #'evil-ex-nohighlight)
    (advice-add #'open-line :after #'auto-indent)
    (dolist (fn '(evil-change evil-delete evil-join evil-paste-after))
      (advice-add fn :after #'auto-indent))
    (setq custom-forward-symbol nil)
    (put 'evil-symbol 'bounds-of-thing-at-point
         (byte-compile
          (lambda ()
            (save-excursion
              (let* ((fwd-sym (or custom-forward-symbol #'forward-symbol))
                     (point (point))
                     (end   (progn (funcall fwd-sym  1) (point)))
                     (start (progn (funcall fwd-sym -1) (point))))
                (if (and (not (= start point end))
                         (<= start point end))
                    (cons start end)
                  (cons point (1+ point))))))))))

(defun spacemacs-ext/post-init-evil-surround ()
  (use-package evil-surround
    :defer t
    :config
    (advice-add #'evil-surround-region :filter-args
                (byte-compile
                 (lambda (args)
                   (if (> 4 (length args))
                       args
                     (let* ((char (nth 3 args))
                            (new-char (cond
                                       ((= 33554474 char) 35) ;;<S-kp-multiply> => #
                                       ((= 33554479 char) 92) ;;<S-kp-divide> => \
                                       ((= 33554477 char) 95) ;;<S-kp-subtract> => _
                                       ((= 33554475 char) 61) ;;<S-kp-add> => =
                                       )))
                       (if new-char
                           (-replace-at 3 new-char args)
                         args))))))
    (advice-add #'evil-surround-change :filter-args
                (byte-compile
                 (lambda (args)
                   (interactive (list (read-key)))
                   args)))
    (advice-add #'evil-surround-delete :filter-args
                (byte-compile
                 (lambda (args)
                   (interactive (list (read-key)))
                   args)))))

(defun spacemacs-ext/post-init-hl-todo ()
  (use-package hl-todo
    :defer t
    :config
    (setq hl-todo-keywords
          (list (list (caar hl-todo-keywords)
                      `(1 (hl-todo-get-face) prepend))))
    (remove-hook 'text-mode-hook #'hl-todo-mode)
    (advice-add #'hl-todo-get-face :filter-return
                (byte-compile
                 (lambda (ret)
                   (list ret))))))

(defun spacemacs-ext/post-init-linum ()
  (use-package linum
    :defer t
    :config
    (add-hook 'linum-mode-hook
              (lambda ()
                (make-local-variable 'window-initial-margins)))))

(defun spacemacs-ext/post-init-linum-relative ()
  (use-package linum-relative
    :defer t
    :config
    (setq linum-delay 0.1
          linum-schedule-timer nil)
    (advice-add #'linum-schedule :override
                (byte-compile
                 (lambda ()
                   (unless (eq 'self-insert-command this-command)
                     (when linum-schedule-timer
                       (cancel-timer linum-schedule-timer))
                     (let ((timer (run-with-idle-timer
                                   linum-delay nil
                                   (lambda ()
                                     (setq linum-schedule-timer nil)
                                     (linum-update-current)))))
                       (setq-local linum-schedule-timer timer))))))
    (add-hook 'find-file-hook
              (lambda ()
                (setq-local linum-relative-format
                            (concat "%"
                                    (-> (count-lines (point-min) (point-max))
                                        (number-to-string)
                                        (length)
                                        (min 5)
                                        (max 3)
                                        (number-to-string))
                                    "s"))))))

(defun spacemacs-ext/post-init-persp-mode ()
  (use-package persp-mode
    :defer t
    :config
    (advice-add #'persp-kill :before
                (lambda (&rest _)
                  "Kill all buffer in the layout."
                  (-when-let (persp (get-current-persp))
                    (dolist (buf (aref persp 2))
                      (kill-buffer-if-not-modified buf)))))))

(defun spacemacs-ext/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    ;; NOTE
    ;; The result of `projectile-sort-by-recentf-first' contain recently visited files.
    ;; So, you will see ignored files.
    (setq projectile-sort-order 'default)
    (advice-add #'projectile-switch-project-by-name :after
                (lambda (&rest _)
                  "Improves `spacemacs/helm-persp-switch-project'."
                  (when (and (featurep 'persp-mode)
                             (get-current-persp))
                    (let* ((root (aref (get-current-persp) 1))
                           (root_ (if (string-prefix-p "/" root)
                                      (concat "~/" (file-relative-name root (getenv "HOME")))
                                    (file-truename root))))
                      (->> (buffer-list)
                           (--filter (or (projectile-project-buffer-p it root_)
                                         (projectile-project-buffer-p it root)))
                           (-map #'persp-add-buffer))))))))

(defun spacemacs-ext/post-init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :config
    (setq rainbow-delimiters--prefix-str (concat "@" "?" "#" "_" "'" "`"))
    (advice-add #'rainbow-delimiters--apply-color :override
                (byte-compile
                 (lambda (loc depth match)
                   (let ((face (funcall rainbow-delimiters-pick-face-function depth match loc))
                         (start loc)
                         (end (1+ loc)))
                     (when face
                       (save-excursion
                         (goto-char start)
                         (when (looking-at-p "\\s(")
                           (skip-chars-backward rainbow-delimiters--prefix-str)
                           (setq start (point))))
                       (font-lock-prepend-text-property start end 'face face))))))))

(defun spacemacs-ext/post-init-smartparens()
  (use-package smartparens
    :defer t
    :config
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          sp-show-pair-from-inside nil
          wrap-sp-supported-modes '(clojure-mode
                                    cider-repl-mode
                                    emas-lisp-mode))
    (advice-add #'sp-newline :after #'auto-indent)))

(defun spacemacs-ext/post-init-spaceline ()
  (use-package spaceline-config
    :defer t
    :init
    ;; NOTE:
    ;;  see https://github.com/syl20bnr/spacemacs/commit/665c09b684a58279f235937b87c643598b0cd33a
    (setq powerline-image-apple-rgb nil)))

(defun spacemacs-ext/post-init-which-key ()
  (use-package which-key
    :defer t
    :config
    (setq which-key-dont-use-unicode t)
    (push '(("\\(.*\\)`" . "winum-select-window-by-number") .
            ("\\1`" . "select window by number"))
          which-key-replacement-alist)))

(defun spacemacs-ext/post-init-whitespace ()
  (use-package whitespace
    :defer t
    :init
    (advice-add #'whitespace-turn-on  :before
                (lambda (&rest _)
                  (when (featurep 'nlinum-mode)
                    (nlinum-mode 0))))
    (advice-add #'whitespace-turn-off :before
                (lambda (&rest _)
                  (when (featurep 'nlinum-mode)
                    (nlinum-mode 1))))))

;;; packages.el ends here

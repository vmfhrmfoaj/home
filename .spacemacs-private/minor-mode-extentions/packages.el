;;; packages.el --- minor-mode-extentions layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst minor-mode-extentions-packages
  '(
    auto-highlight-symbol
    company
    evil
    git-gutter-fringe+
    helm
    helm-projectile
    linum-relative
    smartparens
    ))

(defun minor-mode-extentions/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :config
    (setq ahs-include '((clojure-mode . "[^ \r\t\n]+")
                        (clojurescript-mode . "[^ \r\t\n]+")
                        (clojurec-mode . "[^ \r\t\n]+")
                        (emacs-lisp-mode . "[^ \r\t\n]+")))))

(defun minor-mode-extentions/post-init-company ()
  (use-package company
    :config
    (setq company-idle-delay 5)
    (add-hook 'company-completion-started-hook
              (lambda (&rest _)
                (company-abort)
                (completion-at-point)
                (when (and (bound-and-true-p auto-dim-other-buffers-mode)
                           (fboundp 'adob--dim-buffer))
                  (adob--dim-buffer nil))))))

(defun minor-mode-extentions/post-init-evil ()
  (use-package evil
    :bind (:map evil-motion-state-map
                ("g S-<kp-subtract>" . evil-last-non-blank))
    :config
    (setq auto-indent-skip-when-open-file t)
    (let ((auto-indent
           (lambda (&rest _)
             "auto-indent-for-evil-mode"
             (unless auto-indent-skip-when-open-file
               (save-match-data
                 (ignore-errors
                   (save-excursion
                     (let ((beg (progn
                                  (sp-backward-up-sexp)
                                  (point)))
                           (end (progn
                                  (sp-forward-sexp)
                                  (point))))
                       (indent-region beg end))))))
             (setq-local auto-indent-skip-when-open-file nil))))
      (add-hook 'evil-normal-state-entry-hook auto-indent)
      (dolist (fn '(evil-change
                    evil-delete
                    evil-paste-after
                    evil-join))
        (advice-add fn :after auto-indent)))))

(defun minor-mode-extentions/post-init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :config
    (let ((bitmap (-repeat (line-pixel-height)
                           (apply #'concat (-repeat 7 "X")))))
      (define-fringe-bitmap 'git-gutter-fr+-added
        (apply #'fringe-helper-convert bitmap) nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr+-deleted
        (apply #'fringe-helper-convert bitmap) nil nil nil)
      (define-fringe-bitmap 'git-gutter-fr+-modified
        (apply #'fringe-helper-convert bitmap) nil nil nil))
    (set-face-attribute 'git-gutter-fr+-added    nil :inherit font-lock-string-face)
    (set-face-attribute 'git-gutter-fr+-modified nil :inherit font-lock-function-name-face)
    (set-face-attribute 'git-gutter-fr+-deleted  nil :inherit font-lock-type-face)))

(defun minor-mode-extentions/post-init-helm ()
  (use-package helm
    :bind (("C-h" . delete-backward-char))
    :config
    (setq helm-truncate-lines t)))

(defun minor-mode-extentions/post-init-helm-projectile ()
  (use-package helm-projectile
    :bind (:map helm-projectile-find-file-map
                ("C-h" . delete-backward-char))))

(defun minor-mode-extentions/post-init-linum-relative ()
  (use-package linum-relative
    :config
    (add-hook 'prog-mode-hook (-partial #'linum-relative-mode 1))
    (let ((height (face-attribute 'default :height)))
      (custom-set-faces
       `(linum ((t :underline nil :height ,height)))
       `(linum-relative-current-face ((t :underline nil :height ,height)))))))

(defun minor-mode-extentions/post-init-smartparens()
  (use-package smartparens
    :config
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          wrap-sp-supported-modes
          '(
            clojure-mode
            cider-repl-mode
            emas-lisp-mode
            ))

    (when (require 'smartparens-clojure nil t)
      (defun wrap-sp-forward-symbol (&optional arg)
        (save-match-data
          (when (and (numberp arg)
                     (> arg 0)
                     (apply #'derived-mode-p wrap-sp-supported-modes)
                     (-some->> (buffer-substring (point) (line-end-position))
                               (string-match (concat "^\\s-*"
                                                     sp-clojure-prefix
                                                     "[^({\\[]"))))
            (goto-char (+ (point) (match-end 0))))))
      (defun wrap-sp-backward-symbol (&optional arg)
        (save-match-data
          (when (and (numberp arg)
                     (> arg 0)
                     (apply #'derived-mode-p wrap-sp-supported-modes)
                     (-some->> (buffer-substring (line-beginning-position) (point))
                               (string-match (concat sp-clojure-prefix
                                                     "\\s-*$"))))
            (beginning-of-line)
            (goto-char (+ (point) (match-beginning 0))))))
      (defun wrap-sp-forward-sexp (&optional arg)
        (save-match-data
          (when (and (numberp arg)
                     (> arg 0)
                     (apply #'derived-mode-p wrap-sp-supported-modes)
                     (-some->> (char-after)
                               (char-to-string)
                               (string-match "\\s(")))
            (forward-sexp))))
      (defun wrap-sp-backward-sexp (&optional arg)
        (save-match-data
          (when (and (numberp arg)
                     (> arg 0)
                     (apply #'derived-mode-p wrap-sp-supported-modes)
                     (-some->> (buffer-substring (line-beginning-position) (point))
                               (string-match (concat sp-clojure-prefix "\\s-*$"))))
            (beginning-of-line)
            (goto-char (+ (point) (match-beginning 0))))))
      (advice-add #'sp-forward-symbol :before #'wrap-sp-forward-symbol)
      (advice-add #'sp-backward-symbol :after #'wrap-sp-backward-symbol)
      (advice-add #'sp-forward-sexp :after #'wrap-sp-forward-sexp)
      (advice-add #'sp-backward-sexp :after #'wrap-sp-backward-sexp))))

;; packages.el ends here

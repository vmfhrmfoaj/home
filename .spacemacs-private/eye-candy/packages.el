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
                      ((eq 'emacs-lisp-mode major-mode) -0.1)
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
                                  (second)))
        (powerline-raw (-> "git-branch"
                           (all-the-icons-octicon :v-adjust 0.05)
                           (propertize 'face nil)
                           (concat "·" branch)))))))

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
    (global-prettify-symbols-mode)))

;;; packages.el ends here

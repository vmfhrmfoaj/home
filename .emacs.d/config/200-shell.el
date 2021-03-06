;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'fish-completion nil t)
  (require 'sh-script nil t)
  (require 'eshell nil t))

(use-package fish-completion
  :if (executable-find "fish")
  :ensure t
  :defer t
  :config
  (defun fish-completion--custom-list-completions (raw-prompt)
    "Customize"
    (->> raw-prompt
         (fish-completion--list-completions-with-desc)
         (s-split "\n")
         (--remove (s-blank-p it))
         (--map (-let [(c desc) (split-string it "\t")]
                  (if desc
                      (propertize c :fish-help-message (concat " (" desc ")"))
                    c)))))

  (defun fish-completion--get-help-message (candidate)
    (get-pos-property 0 :fish-help-message candidate))

  (defvar pcomplete-annotation-function nil)

  ;; NOTE
  ;;  Need to install `bash-completion'.
  (setq fish-completion-fallback-on-bash-p t
        pcomplete-annotation-function #'fish-completion--get-help-message)

  (advice-add #'fish-completion--list-completions :override #'fish-completion--custom-list-completions)
  (advice-add #'pcomplete-completions-at-point :filter-return
              (lambda (rv)
                "Add :annotation-function function"
                (if (and (listp rv) (functionp pcomplete-annotation-function))
                    (append rv `(:annotation-function ,pcomplete-annotation-function))
                  rv))))

(use-package sh-script
  :defer t
  :mode ("\\.env\\'" . sh-mode)
  :config
  (setq sh-basic-offset 4
        smie-indent-basic 4))

(use-package eshell
  :defer t
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (fish-completion-mode 1)
              (smartparens-mode 1)
              (show-smartparens-mode 1)
              (setq-local completion-ignore-case t
                          process-connection-type nil)
              (unless (file-remote-p default-directory)
                (apply #'eshell/addpath exec-path))
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          (let ((last-prompt-pos (save-excursion
                                                   (goto-char (point-max))
                                                   (when (re-search-backward eshell-prompt-regexp nil t)
                                                     (match-end 0)))))
                            (when (and last-prompt-pos
                                       (< (point) last-prompt-pos))
                              (goto-char (point-max)))))
                        nil t)))

  :config
  (defun ivy-eshell-history ()
    (interactive)
    (let* ((-compare-fn #'string-equal)
           (history (-some->> eshell-history-ring
                              (ring-elements)
                              (--map (s-trim (substring-no-properties it)))
                              (-non-nil)
                              (-distinct))))
      (ivy-read "Command history:"
                history
                :action (lambda (str)
                          (end-of-buffer)
                          (eshell-kill-input)
                          (insert str))))))

(use-package eshell-prompt-extras
  :ensure t
  :after esh-opt
  :config
  (setq eshell-prompt-function 'epe-theme-lambda))

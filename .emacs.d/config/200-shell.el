;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.el"))

(use-package fish-completion
  :if (executable-find "fish")
  :ensure t
  :init
  (eval-when-compile (require 'fish-completion nil t))

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
                  rv)))

  (global-fish-completion-mode 1))

(use-package sh-script
  :defer t
  :mode ("\\.env\\'" . sh-mode)
  :init
  (eval-when-compile (require 'sh-script nil t))

  :config
  (setq sh-basic-offset 4
        smie-indent-basic 4))

(use-package eshell
  :defer t
  :init
  (eval-when-compile (require 'eshell nil t))

  (add-hook 'eshell-mode-hook
            (lambda ()
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
  (advice-add #'eshell :around
              (lambda (fn &rest args)
                "If single window, create a window and then open eshell buffer on the window."
                (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'pop-to-buffer))
                  (apply fn args)))))

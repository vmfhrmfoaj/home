(setq ring-bell-function 'ignore
      initial-major-mode 'org-mode
      initial-scratch-message "")

(prefer-coding-system 'utf-8)
(when window-system
  (include-shell-var-in "~/.profile"))

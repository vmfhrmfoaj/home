(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t
        solarized-scale-org-headlines nil
        solarized-use-more-italic t
        x-underline-at-descent-line t)
  (load-theme 'solarized-light t)
  (let ((local-var-fg-color (saturate-color (color-from 'font-lock-variable-name-face :foreground 10) -15)))
    (custom-theme-set-faces
     'solarized-light
     '(variable-pitch ((t (:font-family "Monospace"))))
     `(lisp-local-binding-variable-name-face ((t (:foreground ,local-var-fg-color))))
     `(php-passive-assign-variable-face ((t (:foreground ,local-var-fg-color)))))))

(use-package company
  :defer t
  :config
  (custom-set-faces
   `(company-tooltip-selection ((t (:background ,(color-from 'company-tooltip :background -3)))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   '(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil-goggles
  :defer t
  :config
  (custom-set-faces
   '(evil-goggles-change-face ((t (:inherit diff-refine-changed))))
   '(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
   '(evil-goggles-paste-face  ((t (:inherit diff-refine-added))))
   '(evil-goggles-yank-face   ((t (:inherit diff-refine-added))))))

(use-package linum-relative
  :defer t
  :config
  (custom-set-faces
   `(linum-relative-current-face ((t (:weight bold :foreground ,(color-from 'linum :foreground -10)))))))


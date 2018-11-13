(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(variable-pitch ((t (:font-family "Monospace"))))
   `(hl-line ((t (:background ,(light-color "#383838" 1.5)))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,(color-from 'font-lock-variable-name-face :foreground 5)))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   `(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil-goggles
  :defer t
  :config
  (custom-set-faces
   `(evil-goggles-change-face ((t (:inherit diff-refine-changed))))
   `(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
   `(evil-goggles-paste-face  ((t (:inherit diff-refine-added))))
   `(evil-goggles-yank-face   ((t (:inherit diff-refine-added))))))

(use-package git-gutter-fringe+
  :defer t
  :config
  (custom-set-faces
   `(git-gutter+-added    ((t (:foreground ,(color-from 'diff-refine-added   :background 0)))))
   `(git-gutter+-modified ((t (:foreground ,(color-from 'diff-refine-changed :background 0)))))
   `(git-gutter+-deleted  ((t (:foreground ,(color-from 'diff-refine-removed :background 0)))))))

(use-package org
  :defer t
  :config
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :inherit 'variable-pitch))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:foreground ,(color-from 'font-lock-variable-name-face :foreground 5)))))))

(use-package linum-relative
  :defer t
  :config
  (custom-set-faces
   `(linum
     ((t (:inherit default :underline nil :height 1.0 :background ,(color-from 'fringe :background 0)))))
   `(linum-relative-current-face
     ((t (:inherit linum :weight bold :foreground ,(color-from 'linum :foreground 5)))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   `(magit-diff-added-highlight   ((t (:background ,(saturate-color (color-from 'diff-refine-added   :background -5) -10)))))
   `(magit-diff-removed-highlight ((t (:background ,(saturate-color (color-from 'diff-refine-removed :background -5) -10)))))))

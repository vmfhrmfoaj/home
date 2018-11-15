(use-package zenburn-theme
  :ensure t
  :config
  (defvar local-variable-name-fg-color
    (color-from 'font-lock-variable-name-face :foreground 5)
    "TODO")
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(hl-line ((t (:background ,(light-color "#383838" 1.5)))))
   `(lisp-local-binding-variable-name-face ((t (:foreground ,local-variable-name-fg-color))))
   `(trailing-whitespace ((t (:underline "#CC9393"))))
   `(whitespace-trailing ((t (:underline "#CC9393"))))
   `(whitespace-tab      ((t (:underline "#CC9393"))))
   `(whitespace-space ((t (:foreground "#5F5F5F")))))
  (custom-set-faces
   `(lazy-highlight ((t (:background ,(color-from 'hl-line :background -3)))))))

(use-package elixir-mode
  :defer t
  :config
  (custom-set-faces
   `(elixir-atom-face ((t (:inherit 'font-lock-builtin-face))))))

(use-package evil
  :defer t
  :config
  (let* ((hbar-height (max line-spacing 1))
         (hbar-height (if (string-equal "gnome-imac" hostname)
                          (* 2 hbar-height) ; for HiDPI
                        hbar-height)))
    (setq cursor-type 'box
          evil-normal-state-cursor   `(box "#DCDCCC")
          evil-insert-state-cursor   `((hbar . ,hbar-height))
          evil-visual-state-cursor   `(box "#777777")
          evil-replace-state-cursor  `((hbar . ,hbar-height) "#DD9393")
          evil-operator-state-cursor `(hollow "#A6E5E7"))
    (with-eval-after-load 'evil-multiedit
      (setq evil-multiedit-normal-state-cursor `(box "#DCDCCC")
            evil-multiedit-insert-state-cursor `((hbar . ,hbar-height))))))

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
  (custom-set-faces
   `(variable-pitch ((t (:font-family "Monospace")))))
  (dolist (i (number-sequence 1 8))
    (let ((face (intern (concat "org-level-" (number-to-string i)))))
      (set-face-attribute face nil :inherit 'variable-pitch))))

(use-package php-mode
  :defer t
  :config
  (custom-set-faces
   `(php-passive-assign-variable-face ((t (:foreground ,local-variable-name-fg-color))))))

(use-package highlight-parentheses
  :defer t
  :config
  (setq hl-paren-colors
        (--iterate (dim-color it 10)
                   (apply 'color-rgb-to-hex-2-dig (color-name-to-rgb "Springgreen"))
                   4)))

(use-package paren
  :defer t
  :config
  (custom-set-faces
   `(show-paren-match ((t (:background unspecified :foreground "Springgreen" :underline "Springgreen"))))
   `(show-paren-mismatch ((t (:background unspecified :foreground "Springgreen" :underline "Springgreen" :weight bold))))))

(use-package smartparens
  :defer t
  :config
  (custom-set-faces
   `(sp-show-pair-match-face ((t (:background unspecified :foreground "Springgreen" :underline "Springgreen"))))
   `(sp-show-pair-mismatch-face ((t (:background unspecified :foreground "Springgreen" :underline "Springgreen" :weight bold))))))

(use-package rainbow-delimiters
  :defer t
  :config
  (custom-set-faces
   `(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "#a21f20" :weight bold)))))
  (dolist (i (number-sequence 1 9))
    (let ((face (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))))
      (set-face-attribute face nil :foreground
                          (-> face
                              (face-attribute :foreground)
                              (saturate-color -10))))))

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

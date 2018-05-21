(use-package all-the-icons
  :ensure t
  :defer t)

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package fancy-narrow
  :ensure t
  :defer t
  :commands (fancy-narrow-to-defun
             fancy-narrow-to-page
             fancy-narrow-to-region
             fancy-widen))

(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keywords
        (list (list (caar hl-todo-keywords)
                    `(1 (hl-todo-get-face) prepend))))
  (advice-add #'hl-todo-get-face :filter-return #'list)
  (global-hl-todo-mode 1))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  :after evil
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'vi-tilde-fringe nil 'noerror)
    (quelpa '(vi-tilde-fringe :repo "syl20bnr/vi-tilde-fringe" :fetcher github)))

  :config
  (global-vi-tilde-fringe-mode))

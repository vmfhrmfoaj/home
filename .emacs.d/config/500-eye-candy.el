(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package fancy-narrow
  :ensure t)

(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'text-mode-hook #'highlight-numbers-mode))

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

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'vi-tilde-fringe nil 'noerror)
    (el-get-bundle syl20bnr/vi-tilde-fringe))
  :config
  (global-vi-tilde-fringe-mode))

(use-package all-the-icons
  :ensure t
  :config
  (font-lock-add-keywords
   'org-mode
   (let* ((data (all-the-icons-faicon-data))
          (square       (string-to-char (cdr (assoc "square" data))))
          (minus-square (string-to-char (cdr (assoc "minus-square" data))))
          (check-square (string-to-char (cdr (assoc "check-square" data)))))
     `(("^\\s-*\\(-\\) "
        1 (progn
            (let ((s (match-beginning 1))
                  (e (match-end 1)))
              (compose-region s e ?╺))
            'bold))
       ("^\\s-*\\(\\([0-9]\\.\\)\\) "
        1 'bold)
       ("^\\s-*\\(?:-\\|[0-9]+\\.\\) \\(\\[\\( \\|-\\|X\\)\\]\\) "
        1 (progn
            (let ((x (match-string 2))
                  (s (match-beginning 1))
                  (e (match-end 1)))
              (compose-region
               s e
               (cond
                ((string-equal x " ") ,square)
                ((string-equal x "-") ,minus-square)
                ((string-equal x "X") ,check-square)))
              (list :family "FontAwesome"
                    :foreground (face-attribute (if (string-equal x "X")
                                                    'org-done 'org-todo)
                                                :foreground)))) t)
       ("\\(\\\\\\\\\\)\\s-*$"
        1 'shadow nil)))
   'append))

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
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'vi-tilde-fringe nil 'noerror)
    (el-get-bundle syl20bnr/vi-tilde-fringe))
  :config
  (global-vi-tilde-fringe-mode))

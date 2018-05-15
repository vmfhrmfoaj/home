(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1))

(use-package evil-magit
  :ensure t
  :after magit)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vi-tilde-fringe
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (require 'vi-tilde-fringe nil 'noerror)
    (el-get-bundle syl20bnr/vi-tilde-fringe))
  :config
  (global-vi-tilde-fringe-mode))

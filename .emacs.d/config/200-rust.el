(use-package cargo
  :ensure t
  :defer t)

;; NOTE
;;  See, https://github.com/racer-rust/racer#installation
(use-package racer
  :ensure t
  :defer t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode))

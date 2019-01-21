(use-package creole-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.wikicreole\\'" . creole-mode)))

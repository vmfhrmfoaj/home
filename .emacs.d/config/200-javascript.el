(use-package js2-mode
  :ensure t
  :defer t
  :config
  (defconst js--prettify-symbols-alist nil)
  (setq-default js-indent-level 2
                js2-basic-offset 2))

(use-package creole-mode
  ;; :ensure t
  :init
  (unless (package-installed-p 'creole-mode)
    (quelpa '(creole-mode :repo "nicferrier/creole-mode" :fetcher github))))

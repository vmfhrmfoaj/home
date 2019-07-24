(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package queue
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'queue)
    (quelpa '(queue :fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/queue/queue.el" :version original))))

(use-package s
  :ensure t)

(use-package spinner
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'spinner)
    (quelpa '(spinner :fetcher github :repo "Malabarba/spinner.el"))))

(use-package undo-tree
  :defer t
  ;; NOTE:
  ;;  This package not included in the `MELPA'.
  ;;:ensure t
  :init
  (unless (package-installed-p 'undo-tree)
    (quelpa '(undo-tree :fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/undo-tree/undo-tree.el"))))


(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq-local evil-lookup-func
			  (lambda ()
			    (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point)
			    (pop-to-buffer (get-buffer "*Help*")))))))

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (load-file "~/.emacs.d/config/func.elc"))

(use-package css-mode
  :defer t
  :mode "\\.wxss\\'"
  :config

  (defun css--custom-fontify-region (start end &optional loudly)
    "Customize `css--fontify-region' to enhance compatibility with `hl-mode'"
    (let ((extended-region (font-lock-default-fontify-region start end loudly)))
      (when css-fontify-colors
        (when (and (consp extended-region)
		           (eq (car extended-region) 'jit-lock-bounds))
	      (setq start (cadr extended-region))
	      (setq end (cddr extended-region)))
        (save-excursion
	      (let ((case-fold-search t))
	        (goto-char start)
	        (while (re-search-forward css--colors-regexp end t)
	          ;; Skip comments and strings.
	          (unless (nth 8 (syntax-ppss))
	            (let* ((start (match-beginning 0))
                       (color (css--compute-color start (match-string 0))))
		          (when color
		            (with-silent-modifications
		              ;; Use the color as the background, to make it more
		              ;; clear.  Use a contrasting color as the foreground,
		              ;; to make it readable.  Finally, have a small box
		              ;; using the existing foreground color, to make sure
		              ;; it stands out a bit from any other text; in
		              ;; particular this is nice when the color matches the
		              ;; buffer's background color.
		              (add-text-properties
		               start (point)
		               (list 'face (list :background (readable-foreground-color color)
				                         :foreground color
                                         :inverse-video t
				                         :box '(:line-width -1))))))))))))
      extended-region))

  (setq css-indent-offset 2)

  (advice-add #'css--fontify-region :override #'css--custom-fontify-region))

(use-package js
  :defer t
  :config
  (setq-default js-indent-level 4)
  (add-hook 'js-mode-hook
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (with-eval-after-load "lsp-mode"
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))))

(use-package vue-mode
  :disabled t
  :ensure t
  :defer t)

(use-package web-beautify
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.\\(html\\|eex\\|wxml\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (with-eval-after-load "smartparens"
    (add-hook 'web-mode-hook
              (lambda ()
                (-update->> web-mode-auto-pairs
                            (--map (let ((a (car it))
                                        (b (substring (cdr it) 0 -1)))
                                    `(,a . ,b))))))))

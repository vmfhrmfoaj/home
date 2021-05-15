;; -*- lexical-binding: t; -*-

(eval-and-compile (load-file "~/.emacs.d/config/func.el"))

(use-package css-mode
  :defer t
  :mode "\\.wxss\\'"
  :init
  (eval-when-compile (require 'css-mode nil t))

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
  :init
  (eval-when-compile (require 'js nil t))

  :config
  (setq-default js-indent-level 4))

(use-package json-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'json-mode nil t)))

(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'typescript-mode nil t)))

(use-package vue-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'vue-mode nil t)))

(use-package web-beautify
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'web-beautify nil t)))

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.\\(html\\|eex\\|wxml\\)\\'"
  :init
  (eval-when-compile (require 'web-mode nil t))

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (with-eval-after-load "smartparens"
    (add-hook 'web-mode-hook
              (lambda ()
                (prettify-symbols-mode 1)
                (-update->> web-mode-auto-pairs
                            (--map (let ((a (car it))
                                         (b (substring (cdr it) 0 -1)))
                                     `(,a . ,b))))))))

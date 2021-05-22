;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func))

(use-package rpm-spec-mode
  :ensure t
  :defer t
  :mode "\\.rpm\\'"
  :config
  (when (require 'highlight-numbers nil t)
    (puthash 'rpm-spec-mode "\\<[0-9]+\\>" highlight-numbers-modelist))

  (add-hook 'rpm-spec-mode-hook
            (lambda ()
              (setq-local font-lock-multiline t))))

(use-package toml-mode
  :ensure t
  :defer t
  :config
  (defconst toml-syntax-propertize-function
    (syntax-propertize-rules
     ("\\(\\\\\\)\\(?:'\\|'''\\|\"\\|\"\"\"\\)$"
      (1 "w"))
     ("\"\"\"\\|'''"
      (0 (ignore (toml-syntax-stringify))))))

  ;; NOTE
  ;;  copy form `python-mode'
  ;; TODO
  ;;  check unnecessary parts
  (defun toml-syntax-stringify ()
    "Put `syntax-table' property correctly on single/triple quotes."
    (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
           (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
           (quote-starting-pos (- (point) 3))
           (quote-ending-pos (point)))
      (cond
       ((or (nth 4 ppss)                ; Inside a comment
            (and string-start
                 ;; Inside of a string quoted with different triple quotes.
                 (not (eql (char-after string-start)
                           (char-after quote-starting-pos)))))
        ;; Do nothing.
        nil)
       ((nth 5 ppss)
        ;; The first quote is escaped, so it's not part of a triple quote!
        (goto-char (1+ quote-starting-pos)))
       ((null string-start)
        ;; This set of quotes delimit the start of a string.
        (put-text-property quote-starting-pos (1+ quote-starting-pos)
                           'syntax-table (string-to-syntax "|")))
       (t
        ;; This set of quotes delimit the end of a string.
        (put-text-property (1- quote-ending-pos) quote-ending-pos
                           'syntax-table (string-to-syntax "|"))))))

  (add-hook 'toml-mode-hook
            (lambda ()
              (setq-local parse-sexp-lookup-properties t)
              (setq-local syntax-propertize-function toml-syntax-propertize-function))))

(use-package yaml-mode
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode)) ; for systemd service file

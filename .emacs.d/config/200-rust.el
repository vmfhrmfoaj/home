(use-package cargo
  :ensure t
  :defer t)

;; NOTE
;;  See, https://github.com/racer-rust/racer#installation
(use-package racer
  :ensure t
  :defer t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (defn racer--signature-at-point (name)
    "Customize `racer--describe-at-point' to avoid popup the helm buffer to select one of multiple matching."
    (let* ((output-lines (save-excursion
                           ;; Move to the end of the current symbol, to
                           ;; increase racer accuracy.
                           (skip-syntax-forward "w_")
                           (racer--call-at-point "complete-with-snippet")))
           (all-matches (--map (when (s-starts-with-p "MATCH " it)
                                 (racer--split-snippet-match it))
                               output-lines))
           (relevant-matches (--filter (equal (plist-get it :name) name) all-matches))
           (signatures (--map (plist-get it :signature) relevant-matches))
           (signature (unless (< 1 (length signatures))
                        (-first-item signatures))))
      (when (and signature (not (s-starts-with-p "/" signature)))
        signature)))

  (defn racer-eldoc--customized ()
    "Sometime `racer-eldoc' very slow."
    (-when-let (signature (-some-> 'symbol
                                   (thing-at-point)
                                   (racer--signature-at-point)))
      (racer--syntax-highlight signature)))

  :config
  (let ((racer-path (concat (getenv "HOME") "/.cargo/bin/racer"))
        (rust-src (concat (getenv "HOME") "/Desktop/Open_Sources/rust/src")))
    (when (file-exists-p rust-src)
      (setq racer-rust-src-path rust-src)
      (setenv "RUST_SRC_PATH" rust-src)
      (add-to-list 'process-environment (concat "RUST_SRC_PATH=" rust-src)))
    (when (file-executable-p racer-path)
      (setq racer-cmd racer-path)))
  (advice-add 'racer-eldoc :override #'racer-eldoc--customized))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'racer-describe)
              (racer-mode 1)))

  :config
  (defvar cargo-home (or (getenv "CARGO_HOME")
                         (concat (getenv "HOME") "/.cargo"))))

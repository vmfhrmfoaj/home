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

  (defun racer-eldoc--customized ()
    "Sometime `racer-eldoc' very slow."
    (-when-let (describe (-> 'symbol
                             (thing-at-point)
                             (racer--describe-at-point)))
      (racer--syntax-highlight (plist-get describe :signature))))

  :config
  (let ((asdf (concat (getenv "HOME") "/.asdf/bin/asdf"))
        (rust-src (concat (getenv "HOME") "/Desktop/Open_Sources/rust/src")))
    (when (file-exists-p rust-src)
      (setq racer-rust-src-path rust-src)
      (setenv "RUST_SRC_PATH" rust-src)
      (add-to-list 'process-environment (concat "RUST_SRC_PATH=" rust-src)))
    (when (file-executable-p asdf)
      (let ((recer (-> asdf
                       (concat " where rust")
                       (shell-command-to-string)
                       (s-trim)
                       (concat "/bin/racer"))))
        (when (file-executable-p recer)
          (setq racer-cmd recer)))))
  (advice-add 'racer-eldoc :override #'racer-eldoc--customized))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local evil-lookup-func #'racer-describe)
              (racer-mode 1))))

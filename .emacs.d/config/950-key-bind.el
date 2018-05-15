;; for HHKB
(global-set-key (kbd "<S-kp-multiply>") "#")
(global-set-key (kbd "<S-kp-divide>") "\\")
(global-set-key (kbd "<S-kp-subtract>") "_")
(global-set-key (kbd "<S-kp-add>") "=")
(define-key input-decode-map (kbd "<S-kp-multiply>") "#")
(define-key input-decode-map (kbd "<S-kp-divide>") "\\")
(define-key input-decode-map (kbd "<S-kp-subtract>") "_")
(define-key input-decode-map (kbd "<S-kp-add>") "=")

;; Hangul
(global-set-key (kbd "S-SPC") #'toggle-input-method)
(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (local-set-key (kbd "C-h")   #'backward-delete-char)
	    (local-set-key (kbd "S-SPC") #'toggle-input-method)))

(use-package company
  :defer t
  :config
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package evil
  :defer t
  :config
  (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char))

(use-package evil-leader
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC TAB") #'switch-to-previous-buffer)
  (evil-leader/set-key
    "<SPC>" #'helm-M-x
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "bb" #'helm-buffers-list
    "bd" #'evil-delete-buffer
    "bk" #'kill-buffer
    "ff" #'helm-find-files
    "gs" #'magit-status
    "jn" #'sp-newline
    "jo" #'open-line
    "pd" #'helm-projectile-find-dir
    "pf" #'helm-projectile-find-file
    "pp" #'helm-projectile-switch-project
    "rl" #'helm-resume
    "qq" #'save-buffers-kill-terminal))

(use-package helm-mode
  :defer t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key helm-map (kbd "C-j") #'helm-next-line) 
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-comp-read-map (kbd "C-h") #'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-u") #'helm-find-files-up-one-level))

(use-package which-key
  :defer t
  :config
  (which-key-declare-prefixes
    (concat evil-leader/leader "f") "file"
    (concat evil-leader/leader "b") "buffer"
    (concat evil-leader/leader "j") "jump/join/split"
    (concat evil-leader/leader "g") "git"
    (concat evil-leader/leader "p") "project"
    (concat evil-leader/leader "r") "registers/rings/resume"
    (concat evil-leader/leader "q") "quit"))

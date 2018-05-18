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

(use-package bind-map
  :ensure t
  :after evil-leader
  :config
  (defun evil-leader/set-major-leader-for-mode (leader mode &optional evil-states)
    (let ((leaders (list leader))
          (modes (list mode))
          (states (or evil-states '(normal motion visual)))
          (map-name (intern (format "evil-leader-for-%s-map" mode))))
      (-when-let (map (-some->> evil-leader--mode-maps
                                (assoc mode)
                                (-drop 1)
                                (assoc 109) ; 109 = "m"
                                (-drop 1)))
        (eval
         `(progn
            (defvar ,map-name ',map)
            (bind-map ,map-name
              :evil-keys ,leaders
              :evil-states ,states
              :major-modes ,modes)))
        which-key-replacement-alist

        ;; TODO
        ;;  prefix
        )))
  )

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1)
  (evil-leader/set-key
    "<SPC>" #'helm-M-x
    "0" #'winum-select-window-0
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "5" #'winum-select-window-5
    "6" #'winum-select-window-6
    "7" #'winum-select-window-7
    "8" #'winum-select-window-8
    "9" #'winum-select-window-9

    ;; buffer
    "bR" #'revert-buffer
    "bb" #'helm-mini
    "bd" #'evil-delete-buffer
    "bk" #'kill-buffer
    "bs" #'get-scratch-buffer-create

    ;; file
    "ff" #'helm-find-files
    "ft" #'neotree

    ;; git
    "gs" #'magit-status
    "gt" #'git-timemachine

    ;; jump/join/split
    "jn" #'sp-newline
    "jo" #'open-line
    "js" #'sp-split-sexp

    ;; S-expression
    ;; - https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
    "kB" #'sp-backward-barf-sexp
    "kE" #'sp-splice-sexp-killing-forward
    "kR" #'sp-splice-sexp
    "kS" #'sp-backward-slurp-sexp
    "kb" #'sp-forward-barf-sexp
    "ke" #'sp-splice-sexp-killing-backward
    "kr" #'sp-splice-sexp-killing-around
    "ks" #'sp-forward-slurp-sexp
    "kw" #'sp-wrap-sexp

    ;; narrow
    "nf" #'fancy-narrow-to-defun
    "np" #'fancy-narrow-to-page
    "nr" #'fancy-narrow-to-region
    "nw" #'fancy-widen

    ;; project
    "pI" #'projectile-invalidate-cache
    "pd" #'helm-projectile-find-dir
    "pf" #'helm-projectile-find-file
    "pl" #'helm-persp-create-&-switch-project
    "pp" #'helm-projectile-switch-project

    ;; register/rings/resume
    "rl" #'helm-resume

    ;; search/symbol
    "se" #'evil-multiedit-match-all
    "sf" #'helm-do-ag
    "sp" #'helm-do-ag-project-root
    "ss" #'helm-swoop

    ;; toggle
    "tw" #'whitespace-mode
    "tl" #'toggle-truncate-lines

    ;; layout
    "lh" #'persp-switch-to-default
    "lx" #'persp-kill-cur-persp
    "ll" #'helm-persp

    ;; quit
    "qq" #'save-buffers-kill-terminal

    ;; window
    "wH" #'windmove-left
    "wJ" #'windmove-down
    "wK" #'windmove-up
    "wL" #'windmove-right
    "wd" #'delete-window
    "wm" #'delete-other-windows)
  (global-evil-leader-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-declare-prefixes
    (concat evil-leader/leader "f") "file"
    (concat evil-leader/leader "b") "buffer"
    (concat evil-leader/leader "j") "jump/join/split"
    (concat evil-leader/leader "g") "git"
    (concat evil-leader/leader "k") "S-expression"
    (concat evil-leader/leader "m") "major mode keys"
    (concat evil-leader/leader "n") "narrow"
    (concat evil-leader/leader "p") "project"
    (concat evil-leader/leader "r") "registers/rings/resume"
    (concat evil-leader/leader "s") "search/symbol"
    (concat evil-leader/leader "t") "toggle"
    (concat evil-leader/leader "q") "quit"
    (concat evil-leader/leader "w") "window"))


;; Key binding for the minor mode

(use-package company
  :defer t
  :config
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (evil-global-set-key 'insert (kbd "TAB") #'company-indent-or-complete-common))

(use-package evil
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC TAB") #'switch-to-previous-buffer)
  (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char))

(use-package git-timemachine
  :defer t
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "C-j") #'git-timemachine-show-previous-revision)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "C-k") #'git-timemachine-show-next-revision)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "M-b") #'git-timemachine-blame)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "M-w") #'git-timemachine-kill-abbreviated-revision)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "M-W") #'git-timemachine-kill-revision)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "q")   #'git-timemachine-quit))

(use-package helm-company
  :after company
  :config
  (define-key company-active-map (kbd "C-s") #'helm-company))

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


;; Key binding for the major mode

(use-package elisp-mode
  :defer t
  :config
  ;; emacs-lisp-mode
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "mee" #'eval-last-sexp
    "mef" #'eval-defun
    "mgg" #'elisp-slime-nav-find-elisp-thing-at-point
    "mrs" #'emacs-lisp-REPL-buffer)
  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto definition"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode "," 'emacs-lisp-mode)

  ;; lisp-interaction-mode
  (evil-leader/set-key-for-mode 'lisp-interaction-mode
    "mgg" #'elisp-slime-nav-find-elisp-thing-at-point)
  (which-key-declare-prefixes-for-mode 'lisp-interaction-mode
    (concat evil-leader/leader "mg") "goto definition")
  (evil-leader/set-major-leader-for-mode "," 'lisp-interaction-mode)
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "RET")
                                  #'emacs-lisp-REPL-eval-print-this-sexp))
            'append))

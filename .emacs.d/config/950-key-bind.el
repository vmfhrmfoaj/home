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
  :init
  (defun evil-leader/set-major-leader-for-mode (leader mode &optional evil-states)
    "TODO"
    (let ((leaders (list leader))
          (modes (list mode))
          (states (or evil-states '(normal motion visual)))
          (map-name (intern (format "evil-leader-for-%s-map" mode))))
      (-when-let (map (-some->> evil-leader--mode-maps
                                (assoc mode)
                                (-drop 1)
                                (assoc 109) ; 109 = "m"
                                (-drop 1)))
        ;; for evil-leader
        (eval
         `(progn
            (defvar ,map-name ',map)
            (bind-map ,map-name
              :evil-keys ,leaders
              :evil-states ,states
              :major-modes ,modes))))
      ;; for which-key
      (-when-let (it (assoc mode which-key-replacement-alist))
        (let* ((evil-leader (s-chop-prefix "<" (s-chop-suffix ">" evil-leader/leader)))
               (its-vals (cdr it))
               (its-new-vals
                (--map (let ((prefix (caar it))
                             (more (cdr it)))
                         (cons (list (s-replace (concat evil-leader " m") leader prefix)) more))
                       its-vals)))
          (setcdr it (append its-vals its-new-vals)))))))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1)
  (evil-leader/set-key
    "<SPC>" #'helm-M-x
    "TAB" #'switch-to-previous-buffer
    "!" #'shell-command
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
    "u" #'universal-argument

    ;; applications

    ;; - org
    "aoa" #'org-agenda-list
    "aocj" #'org-clock-jump-to-current-clock
    "aocn" #'org-capture-note
    "aoct" #'org-capture-todo
    "aos" #'org-search-view
    "aot" #'org-todo-list

    ;; buffer
    "bR" #'revert-buffer
    "ba" #'persp-add-buffer
    "bb" #'helm-mini
    "bd" #'evil-delete-buffer
    "bk" #'kill-buffer
    "bs" #'get-scratch-buffer-create
    "bl" #'switch-to-previous-buffer

    ;; error
    "en" #'next-error
    "ep" #'previous-error

    ;; file
    "ff" #'helm-find-files
    "ft" #'neotree
    "fy" #'kill-new-buffer-file-name

    ;; git
    "gb" #'magit-blame
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
    "p!" #'projectile-run-shell-command-in-root
    "pI" #'projectile-invalidate-cache
    "pd" #'helm-projectile-find-dir
    "pf" #'helm-projectile-find-file
    "pl" #'helm-persp-create-&-switch-project
    "pp" #'helm-projectile-switch-project
    "pt" #'neotree-project-dir

    ;; register/rings/resume
    "rl" #'helm-resume

    ;; search/symbol
    "se" #'evil-multiedit-match-all
    "sf" #'helm-do-ag
    "sl" #'helm-resume-last-search-buffer
    "sp" #'helm-do-ag-project-root
    "ss" #'helm-swoop

    ;; toggle
    "tw" #'whitespace-mode
    "tl" #'toggle-truncate-lines

    ;; layout
    "l TAB" #'persp-switch-to-last-selected-persp
    "lh" #'persp-switch-to-default
    "lx" #'persp-kill-cur-persp
    "ll" #'helm-persp

    ;; quit
    "qq" #'save-buffers-kill-terminal

    ;; window
    "w-" #'split-window-vertically
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
    (concat evil-leader/leader "a") "applications"
    (concat evil-leader/leader "ao") "org"
    (concat evil-leader/leader "aoc") "capture/clock"
    (concat evil-leader/leader "e") "error"
    (concat evil-leader/leader "f") "file"
    (concat evil-leader/leader "b") "buffer"
    (concat evil-leader/leader "j") "jump/join/split"
    (concat evil-leader/leader "g") "git"
    (concat evil-leader/leader "k") "S-expression"
    (concat evil-leader/leader "l") "layout"
    (concat evil-leader/leader "m") "major-mode-keys"
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
  (define-key company-active-map [return] #'newline)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (evil-global-set-key 'insert (kbd "TAB") #'company-indent-or-complete-common))

(use-package evil
  :defer t
  :config
  (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char))

(use-package evil-surround
  :defer t
  :config
  (evil-define-key 'visual evil-surround-mode-map
    "S" 'evil-substitute
    "s" 'evil-surround-region))

(use-package git-timemachine
  :defer t
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    (kbd "C-j") #'git-timemachine-show-previous-revision
    (kbd "C-k") #'git-timemachine-show-next-revision
    (kbd "M-b") #'git-timemachine-blame
    (kbd "M-w") #'git-timemachine-kill-abbreviated-revision
    (kbd "M-W") #'git-timemachine-kill-revision
    (kbd "q")   #'git-timemachine-quit))

(use-package helm-company
  :after company
  :config
  (define-key company-active-map (kbd "C-s") #'helm-company))

(use-package helm-mode
  :defer t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key helm-map (kbd "C-h") #'delete-backward-char)
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "C-n") #'helm-next-source)
  (define-key helm-map (kbd "C-p") #'helm-previous-source)
  (dolist (map (list helm-find-files-map
                     helm-read-file-map))
    (define-key map (kbd "TAB") #'helm-execute-persistent-action)
    (define-key map (kbd "C-u") #'helm-find-files-up-one-level)))

(use-package neotree
  :defer t
  :config
  (evil-define-key 'normal neotree-mode-map
    (kbd "C-c o") #'neotree-enter-horizontal-split
    (kbd "C-u") #'neotree-select-up-node
    (kbd "RET") #'neotree-enter
    (kbd "+") #'neotree-create-node
    (kbd "R") #'neotree-rename-node
    (kbd "h") #'neotree-back
    (kbd "l") #'neotree-enter
    (kbd "x") #'neotree-delete-node
    (kbd "q") #'evil-delete-buffer))

(use-package magit-svn
  :defer t
  :config
  (evil-magit-define-key 'normal 'magit-mode-map (kbd "~") #'magit-svn-popup))


;; Key binding for the major mode

(use-package cc-mode
  :defer t
  :config
  (dolist (mode '(c-mode c++-mode java-mode))
    (evil-leader/set-key-for-mode mode
      "mgg" #'dumb-jump-go)
    (which-key-declare-prefixes-for-mode mode
      (concat evil-leader/leader "mg") "goto")
    (evil-leader/set-major-leader-for-mode "," mode)))

(use-package cider-repl
  :defer t
  :config
  (evil-leader/set-key-for-mode 'cider-repl-mode
    "mee" #'cider-eval-last-sexp
    "mef" #'cider-eval-defun-at-point
    "mer" #'cider-eval-last-sexp-and-replace
    "mgg" #'cider-find-var
    "mrs" #'cider-switch-to-last-clj-buf
    "mrq" #'cider-quit)
  (which-key-declare-prefixes-for-mode 'cider-repl-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode "," 'cider-repl-mode))

(use-package cider-stacktrace
  :defer t
  :config
  (evil-define-key 'normal cider-stacktrace-mode-map
    (kbd "q") #'evil-delete-buffer))

(use-package clojure-mode
  :defer t
  :config
  (dolist (mode '(clojure-mode clojurec-mode clojurescript-mode))
    (evil-leader/set-key-for-mode mode
      "mee" #'cider-eval-last-sexp
      "mef" #'cider-eval-defun-at-point
      "men" #'cider-eval-ns-form
      "mer" #'cider-eval-last-sexp-and-replace
      "mgg" #'cider-find-var-at-point
      "mrc" #'cider-connect
      "mrs" #'cider-switch-to-releated-repl-buffer
      "mrq" #'cider-quit)
    (which-key-declare-prefixes-for-mode mode
      (concat evil-leader/leader "me") "evaluation"
      (concat evil-leader/leader "mg") "goto"
      (concat evil-leader/leader "mr") "REPL")
    (evil-leader/set-major-leader-for-mode "," mode)))

(use-package cperl-mode
  :defer t
  :config
  (define-key cperl-mode-map (kbd "{") nil) ; disable `cperl-electric-lbrace'
  (evil-define-key 'normal cperl-mode-map (kbd "M-,") #'pop-tag-mark)
  (evil-leader/set-key-for-mode 'cperl-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'cperl-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode "," 'cperl-mode))

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
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode "," 'emacs-lisp-mode)

  ;; lisp-interaction-mode
  (evil-leader/set-key-for-mode 'lisp-interaction-mode
    "mgg" #'elisp-slime-nav-find-elisp-thing-at-point)
  (which-key-declare-prefixes-for-mode 'lisp-interaction-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode "," 'lisp-interaction-mode)
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "RET")
                                  #'emacs-lisp-REPL-eval-print-this-sexp))
            :append))

(use-package org
  :defer t
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "m:" #'org-set-tags
    "mci" #'org-clock-in
    "mco" #'org-clock-out
    "mtI" #'org-time-stamp-inactive
    "mti" #'org-time-stamp
    "mtd" #'org-deadline
    "mts" #'org-schedule
    "mTT" #'org-todo)
  (which-key-declare-prefixes-for-mode 'org-mode
    (concat evil-leader/leader "mT") "todo"
    (concat evil-leader/leader "mc") "clock"
    (concat evil-leader/leader "mt") "time")
  (evil-leader/set-major-leader-for-mode "," 'org-mode))

(use-package php-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'php-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'php-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode "," 'php-mode))

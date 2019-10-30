;; for HHKB
(when HHKB?
  (global-set-key (kbd "<S-kp-multiply>") "#")
  (global-set-key (kbd "<S-kp-divide>") "\\")
  (global-set-key (kbd "<S-kp-subtract>") "_")
  (global-set-key (kbd "<S-kp-add>") "=")
  (define-key input-decode-map (kbd "<S-kp-multiply>") "#")
  (define-key input-decode-map (kbd "<S-kp-divide>") "\\")
  (define-key input-decode-map (kbd "<S-kp-subtract>") "_")
  (define-key input-decode-map (kbd "<S-kp-add>") "="))

;; minibuffer
(define-key isearch-mode-map (kbd "C-g") #'isearch-cancel)
(define-key isearch-mode-map (kbd "C-h")
  (byte-compile
   (lambda ()
     (interactive)
     (if (and isearch-success (not isearch-error))
         (isearch-delete-char)
       (while (or (not isearch-success) isearch-error) (isearch-pop-state))
       (isearch-update)))))
(remove-hook 'minibuffer-setup-hook #'evil-initialize)
(add-hook 'minibuffer-setup-hook
          (byte-compile
           (if evil-want-minibuffer
               (lambda ()
                 (evil-initialize)
                 (set (make-local-variable 'evil-echo-state) nil)
                 (evil-insert 1)
                 (evil-local-set-key 'insert (kbd "C-a") #'beginning-of-line)
                 (evil-local-set-key 'insert (kbd "C-b") #'backward-char)
                 (evil-local-set-key 'insert (kbd "C-h") #'backward-delete-char)
                 (when (and helm-alive-p (string= helm-buffer "*helm find files*"))
                   (evil-local-set-key 'insert (kbd "TAB") #'helm-execute-persistent-action))
                 ;; FIXME
                 ;;  Not working `minibuffer-local-map'
                 (unless (or helm-alive-p
                             evil-ex-current-buffer)
                   (evil-local-set-key 'normal (kbd "<escape>") #'abort-recursive-edit)
                   (evil-local-set-key 'normal (kbd "RET") #'exit-minibuffer)
                   (evil-local-set-key 'insert (kbd "RET") #'exit-minibuffer)))
             (lambda ()
               (local-set-key (kbd "C-a") #'beginning-of-line)
               (local-set-key (kbd "C-b") #'backward-char)
               (local-set-key (kbd "C-h") #'backward-delete-char)))))

(use-package bind-map
  :ensure t
  :after evil-leader
  :init
  (defvar evil-leader/major-leader ","
    "TODO")

  (defn evil-leader/set-major-leader (key)
    "TODO"
    (let ((old-m-leader evil-leader/major-leader))
      (setq evil-leader/major-leader key)
      (dolist (mode (-map #'car evil-leader--mode-maps))
        (evil-leader/set-major-leader-for-mode mode))))

  (defn evil-leader/set-major-leader-for-mode (mode)
    "TODO"
    (let ((map-name (intern (format "evil-leader-for-%s-map" mode))))
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
              :evil-keys (,evil-leader/major-leader)
              :evil-states (normal)
              :major-modes (,mode)))))
      ;; for which-key
      (-when-let (it (assoc mode which-key-replacement-alist))
        (let* ((evil-leader (s-chop-prefix "<" (s-chop-suffix ">" evil-leader/leader)))
               (its-vals (cdr it))
               (its-new-vals
                (--map (let ((prefix (caar it))
                             (more (cdr it)))
                         (cons (list (s-replace (concat evil-leader " m") evil-leader/major-leader prefix)) more))
                       its-vals)))
          (setcdr it (append its-vals its-new-vals))))))

  (defn evil-leader/set-local-key (&rest bindings)
    (let* ((prefix (concat evil-leader/leader "m"))
           (bindings (->> bindings
                          (-partition 2)
                          (--map (cons (concat evil-leader/leader (car it)) (cdr it)))
                          (--mapcat (if (not (s-starts-with? prefix (car it)))
                                        (list it)
                                      (list it (cons (s-replace prefix evil-leader/major-leader (car it)) (cdr it))))))))
      (dolist (binding bindings)
        (evil-local-set-key 'normal (car binding) (cadr binding))))))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-major-leader ",")
  (evil-leader/set-key
    "<SPC>" #'helm-M-x
    "TAB" #'switch-to-previous-buffer
    "!" #'shell-command
    ";" #'comment-it
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
    "aoa" #'org-agenda-show-list
    "aocj" #'org-clock-goto
    "aocn" #'org-capture-note
    "aoct" #'org-capture-todo
    "aom" #'org-tags-view
    "aoM" (defalias 'org-tags-view-todo-only
            (lambda ()
              (interactive)
              (org-tags-view t)))
    "aor" #'org-agenda-resume
    "aos" #'org-search-view
    "aoS" (defalias 'org-search-view-todo-only
            (lambda ()
              (interactive)
              (org-search-view t)))
    "aot" #'org-todo-list

    ;; - calculator
    "ac" #'calc

    ;; - shell
    "as" #'helm-mt

    ;; - undo-tree
    "au" #'undo-tree-visualize

    ;; buffer
    "bR" #'revert-buffer
    "ba" #'persp-add-buffer
    "bb" #'helm-buffers-list
    "bd" #'evil-delete-buffer
    "bk" #'kill-buffer
    "bs" #'get-scratch-buffer-create
    "bl" #'switch-to-previous-buffer

    ;; display
    "dj" #'evil-show-jumps
    "dm" #'evil-show-marks
    "dr" #'evil-show-registers

    ;; error
    "en" #'next-error
    "ep" #'previous-error

    ;; file
    "ff" #'helm-find-files
    "fr" #'helm-recentf
    "ft" #'neotree
    "fy" #'kill-new-buffer-file-name

    ;; git
    "gb" #'magit-blame-addition
    "gS" #'git-gutter+-stage-hunks
    "gs" #'magit-status
    "gt" #'git-timemachine
    "gp" #'git-gutter+-previous-hunk
    "gn" #'git-gutter+-next-hunk

    ;; jump/join/split
    "jn" #'newline-and-indent
    "jo" #'split-line
    "js" #'sp-split-sexp

    ;; S-expression
    ;; - https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
    "kB" #'sp-backward-barf-sexp
    "kE" #'sp-splice-sexp-killing-forward
    "kR" #'sp-splice-sexp
    "kS" #'sp-backward-slurp-sexp
    "kb" #'sp-forward-barf-sexp
    "kc" #'sp-convolute-sexp
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
    "pA" #'projectile-add-known-project
    "pD" #'projectile-remove-known-project
    "pI" #'projectile-invalidate-cache
    "pd" #'helm-projectile-find-dir
    "pf" #'helm-projectile-find-file
    "pl" #'helm-persp-create-&-switch-project
    "pp" #'helm-projectile-switch-project
    "pt" #'neotree-project-dir

    ;; register/rings/resume
    "rk" #'helm-show-kill-ring
    "rl" #'helm-resume

    ;; search/symbol
    "se" #'evil-multiedit-match-all
    "sf" #'helm-do-ag
    "sF" #'helm-do-ag-this-file
    "sg" #'helm-do-grep-ag
    "sr" #'helm-resume-last-search-buffer
    "sp" #'helm-do-ag-project-root
    "ss" #'helm-swoop

    ;; toggle
    "tl" #'toggle-truncate-lines
    "tm" #'toggle-frame-maximized
    "tw" #'whitespace-mode

    ;; layout
    "l TAB" #'persp-switch-to-last-selected-persp
    "l@" #'persp-switch-to-org
    "lh" #'persp-switch-to-default
    "ll" #'helm-persp
    "lr" #'persp-load-state-from-file
    "ls" #'persp-save-state-to-file
    "lx" #'persp-kill-cur-persp

    ;; quit
    "qq" #'save-buffers-kill-emacs

    ;; window
    "w-" #'split-window-vertically
    "wh" #'windmove-left
    "wj" #'windmove-down
    "wk" #'windmove-up
    "wl" #'windmove-right
    "wd" #'delete-window
    "wm" #'delete-other-windows

    ;; text
    "xr" #'align-regexp)
  (when (eq 'darwin system-type)
    (evil-leader/set-key
      ;; apllication
      ;; - dictionary
      "ad" #'osx-dictionary-search-input))
  (global-evil-leader-mode 1)
  ;; NOTE:
  ;;  turn on `evil-leader-mode' for buffers already opened.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (evil-leader-mode 1))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-declare-prefixes
    (concat evil-leader/leader "a") "applications"
    (concat evil-leader/leader "ao") "org"
    (concat evil-leader/leader "aoc") "capture/clock"
    (concat evil-leader/leader "d") "display"
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
  :disabled
  :defer t
  :config
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "SPC") #'company-abort-and-insert-space)
  (evil-global-set-key 'insert (kbd "TAB") #'company-indent-or-complete-common))

(use-package cider-repl
  :defer t
  :config
  (evil-leader/set-key-for-mode 'cider-repl-mode
    "mee" #'cider-eval-last-sexp
    "mef" #'cider-eval-defun-at-point
    "mer" #'cider-eval-last-sexp-and-replace
    "mgg" #'cider-find-var-at-point
    "mrc" #'cider-repl-clear-buffer
    "mrs" #'cider-switch-to-last-clj-buf
    "msn" #'cider-repl-set-ns
    "mrq" #'cider-quit)
  (which-key-declare-prefixes-for-mode 'cider-repl-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "ms") "set/change"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode 'cider-repl-mode))

(use-package cider-stacktrace
  :defer t
  :config
  (evil-define-key 'normal cider-stacktrace-mode-map
    (kbd "q") #'evil-delete-buffer))

(use-package evil
  :defer t
  :config
  (define-key evil-outer-text-objects-map "U" 'evil-a-sexp)
  (define-key evil-inner-text-objects-map "U" 'evil-inner-sexp)
  (evil-define-key 'normal 'global
    "gd" #'up-list
    "gr" #'eldoc-refresh
    "gu" #'backward-up-list
    (kbd "TAB") #'indent-for-tab-command)
  (evil-define-key 'insert 'global
    (kbd "C-h") #'backward-delete-char
    (kbd "C-a") #'beginning-of-line-text
    (kbd "C-e") #'end-of-line)
  (evil-define-key 'visual 'global
    (kbd "TAB") #'indent-region
    (kbd "v") #'er/expand-region))

(use-package evil-ex
  :defer t
  :config
  (define-key evil-ex-completion-map [remap abort-recursive-edit] #'abort-recursive-edit-for-evil-ex)
  (when evil-want-minibuffer
    (evil-define-key 'normal evil-ex-completion-map
      (kbd "<escape>") #'abort-recursive-edit-for-evil-ex)
    (evil-define-key 'insert evil-ex-completion-map
      (kbd "<escape>") (byte-compile
                        (lambda ()
                          (interactive)
                          (if evil-ex-expression
                              (evil-normal-state)
                            (abort-recursive-edit-for-evil-ex)))))))

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
    (kbd "i")   #'evil-normal-state
    (kbd "M-p") #'git-timemachine-show-previous-revision
    (kbd "M-n") #'git-timemachine-show-next-revision
    (kbd "M-b") #'git-timemachine-blame
    (kbd "M-w") #'git-timemachine-kill-abbreviated-revision
    (kbd "M-W") #'git-timemachine-kill-revision
    (kbd "q")   #'git-timemachine-quit))

(use-package helm-ag
  :defer t
  :config
  (define-key helm-ag-map    (kbd "C-u") #'helm-ag--up-one-level)
  (define-key helm-do-ag-map (kbd "C-u") #'helm-ag--do-ag-up-one-level))

(use-package helm-company
  :defer t
  :config
  (evil-global-set-key 'insert (kbd "TAB") #'company-indent-or-helm-company)
  (define-key company-active-map (kbd "C-s") #'helm-company-plus))

(use-package helm-files
  :defer t
  :config
  (dolist (map (list helm-find-files-map
                     helm-read-file-map))
    (define-key map (kbd "C-u") #'helm-find-files-up-one-level)
    (define-key map [C-backspace] #'backward-kill-word)))

(use-package helm-mode
  :defer t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (if evil-want-minibuffer
      (progn
        (evil-define-key 'normal helm-map
          "k" #'helm-previous-line
          "j" #'helm-next-line
          (kbd "C-g") #'helm-keyboard-quit
          (kbd "RET") #'helm-maybe-exit-minibuffer
          (kbd "<escape>") #'helm-keyboard-quit)
        (evil-define-key 'insert helm-map
          (kbd "C-g") #'helm-keyboard-quit
          (kbd "TAB") #'helm-select-action))
    (define-key helm-map (kbd "<escape>") #'helm-keyboard-quit)
    (define-key helm-map (kbd "C-h") #'delete-backward-char)))

(use-package helm-swoop
  :defer t
  :config
  (define-key helm-swoop-edit-map (kbd "C-c C-c") #'helm-swoop--edit-complete)
  (define-key helm-swoop-edit-map (kbd "C-c C-k") #'helm-swoop--edit-cancel))

(use-package help-mode
  :defer t
  :config
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key 'normal help-mode-map
    (kbd "TAB")   #'forward-button
    (kbd "S-TAB") #'backward-button
    (kbd "M-,") #'help-go-back
    (kbd "q") #'quit-window))

(use-package neotree
  :defer t
  :config
  (evil-define-key 'normal neotree-mode-map
    (kbd "C-c o") #'neotree-enter-horizontal-split
    (kbd "C-u") #'neotree-select-up-node
    (kbd "RET") #'neotree-enter
    (kbd "TAB") #'neotree-toggle-maximize
    (kbd "+") #'neotree-create-dir
    (kbd "C") #'neotree-copy-node
    (kbd "R") #'neotree-rename-node
    (kbd "c") #'neotree-create-node
    (kbd "h") #'neotree-back
    (kbd "l") #'neotree-enter
    (kbd "x") #'neotree-delete-node
    (kbd "q") #'neotree-hide))

(use-package magit
  :defer t
  :config
  (define-key transient-base-map (kbd "C-g")      #'transient-quit-all)
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one)
  (define-key transient-map (kbd "C-g")      #'transient-quit-all)
  (define-key transient-map (kbd "<escape>") #'transient-quit-one))

(use-package magit-blame
  :defer t
  :config
  (when (fboundp 'git-timemachine-from-magit-blame)
    (evil-define-key 'normal magit-blame-mode-map
      (kbd "M-t") #'git-timemachine-from-magit-blame)))

(use-package magit-svn
  :after evil-magit
  :config
  (evil-magit-define-key 'normal 'magit-mode-map (kbd "~") #'magit-svn))


;; Key binding for the major mode

(use-package alchemist-help
  :defer t
  :config
  (evil-define-key 'normal alchemist-help-minor-mode-map
    (kbd "q") #'evil-delete-buffer))

(use-package cc-mode
  :defer t
  :config
  (dolist (mode '(c-mode c++-mode java-mode))
    (evil-leader/set-key-for-mode mode
      "mgg" #'lsp-find-definition
      "mgd" #'lsp-find-declaration
      "mgi" #'lsp-find-implementation
      "mgt" #'lsp-find-type-definition)
    (which-key-declare-prefixes-for-mode mode
      (concat evil-leader/leader "mg") "goto")
    (evil-leader/set-major-leader-for-mode mode)))

(use-package clojure-mode
  :defer t
  :config
  (dolist (mode '(clojure-mode clojurec-mode clojurescript-mode))
    (evil-leader/set-key-for-mode mode
      "mee" #'cider-eval-last-sexp
      "meb" #'cider-eval-buffer
      "mef" #'cider-eval-defun-at-point
      "men" #'cider-eval-ns-form
      "mer" #'cider-eval-last-sexp-and-replace
      "mgg" #'cider-find-var-at-point
      "mhh" #'helm-cider-cheatsheet
      "mrc" #'cider-connect
      "mrs" #'cider-switch-to-releated-repl-buffer
      "mrq" #'cider-quit)
    (which-key-declare-prefixes-for-mode mode
      (concat evil-leader/leader "me") "evaluation"
      (concat evil-leader/leader "mg") "goto"
      (concat evil-leader/leader "mh") "help"
      (concat evil-leader/leader "mr") "REPL")
    (evil-leader/set-major-leader-for-mode mode)))

(use-package cperl-mode
  :defer t
  :config
  (define-key cperl-mode-map (kbd "{") nil) ; disable `cperl-electric-lbrace'
  (evil-define-key 'normal cperl-mode-map (kbd "M-,") #'pop-tag-mark)
  (evil-leader/set-key-for-mode 'cperl-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'cperl-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'cperl-mode))

(use-package ediff
  :defer t
  :config
  (advice-add #'ediff-setup-keymap :after
              (byte-compile
               (lambda ()
                 (define-key ediff-mode-map (kbd "J") #'ediff-jump-to-difference)
                 (define-key ediff-mode-map (kbd "k") #'ediff-previous-difference)
                 (define-key ediff-mode-map (kbd "j") #'ediff-next-difference)
                 (define-key ediff-mode-map (kbd "0") #'ediff-reset-text-size)
                 (define-key ediff-mode-map (kbd "+") #'ediff-increase-text-size)
                 (define-key ediff-mode-map (kbd "-") #'ediff-decrease-text-size)))))

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
  (evil-leader/set-major-leader-for-mode 'emacs-lisp-mode)

  ;; lisp-interaction-mode
  (evil-leader/set-key-for-mode 'lisp-interaction-mode
    "mee" #'emacs-lisp-REPL-eval-print-this-sexp
    "mgg" #'elisp-slime-nav-find-elisp-thing-at-point
    "mrc" #'erase-buffer
    "mrq" #'evil-delete-buffer)
  (which-key-declare-prefixes-for-mode 'lisp-interaction-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode 'lisp-interaction-mode))

(use-package elixir-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'elixir-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'elixir-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'elixir-mode))

(use-package elm-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'elm-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'elm-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'elm-mode))

(use-package go-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'go-mode
    "mgg" #'go-guru-definition
    "mgG" #'go-guru-definition-other-window)
  (which-key-declare-prefixes-for-mode 'go-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'go-mode))

(use-package js
  :defer t
  :config
  (evil-leader/set-key-for-mode 'js-mode
    "mgg" #'lsp-find-definition
    "mgd" #'lsp-find-declaration
    "mgi" #'lsp-find-implementation
    "mgt" #'lsp-find-type-definition)
  (which-key-declare-prefixes-for-mode 'js-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'js-mode))

(use-package lsp-mode
  :defer t
  :config
  (add-hook 'lsp-mode-hook
            (lambda ()
              (evil-local-set-key 'normal [remap next-error]     #'flymake-goto-next-error)
              (evil-local-set-key 'normal [remap previous-error] #'flymake-goto-prev-error))))

(use-package multi-term
  :defer t
  :config
  (evil-define-key 'normal term-raw-map
    (kbd "b") #'term-send-ctrl-left
    (kbd "h") #'term-send-left
    (kbd "l") #'term-send-right
    (kbd "w") #'term-send-ctrl-right
    (kbd "x") #'term-send-del)
  (evil-define-key 'insert term-raw-map
    (kbd "C-h") #'term-send-backspace
    (kbd "M-DEL") #'term-send-backward-kill-word))

(use-package org
  :defer t
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "m:" #'org-set-tags-command
    "mTT" #'org-todo
    "mci" #'org-clock-in
    "mco" #'org-clock-out
    "mcj" #'org-clock-goto
    "mih" #'org-insert-heading
    "miH" #'org-insert-subheading
    "mtI" #'org-time-stamp-inactive
    "mtd" #'org-deadline
    "mti" #'org-time-stamp
    "mts" #'org-schedule)
  (which-key-declare-prefixes-for-mode 'org-mode
    (concat evil-leader/leader "mT") "todo"
    (concat evil-leader/leader "mc") "clock"
    (concat evil-leader/leader "mi") "insert"
    (concat evil-leader/leader "mt") "time"
    (concat evil-leader/leader "ms") "sync (trello)")
  (evil-leader/set-major-leader-for-mode 'org-mode)
  (evil-define-key 'normal evil-org-mode-map
    (kbd "RET") #'org-open-at-point
    (kbd "M-,") #'org-mark-ring-goto))

(use-package package
  :defer t
  :config
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-define-key 'normal package-menu-mode-map
    (kbd "U") #'package-menu-mark-upgrades
    (kbd "d") #'package-menu-mark-delete
    (kbd "i") #'package-menu-mark-install
    (kbd "q") #'kill-buffer
    (kbd "x") #'package-menu-execute))

(use-package php-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'php-mode
    "mgg" #'lsp-find-definition
    "mgd" #'lsp-find-declaration
    "mgi" #'lsp-find-implementation
    "mgt" #'lsp-find-type-definition
    "mrr" #'psysh
    "mrR" #'psysh-restart
    "mrs" #'psysh-show)
  (which-key-declare-prefixes-for-mode 'php-mode
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "REPL")
  (evil-leader/set-major-leader-for-mode 'php-mode)
  (define-key php-mode-map [tab] nil))

(use-package profiler
  :defer t
  :config
  (evil-define-key 'normal 'profiler-report-mode-map
    (kbd "TAB") #'profiler-report-expand-entry))

(use-package psysh
  :defer t
  :config
  (evil-define-key 'normal 'psysh-mode-map
    (kbd "M-p") #'comint-previous-input
    (kbd "M-n") #'comint-next-input))

(use-package racer
  :defer t
  :config
  (add-hook 'racer-help-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "q") #'quit-window))))

(use-package rust-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'rust-mode
    "mgg" #'lsp-find-definition
    "mgd" #'lsp-find-declaration
    "mgi" #'lsp-find-implementation
    "mgt" #'lsp-find-type-definition)
  (which-key-declare-prefixes-for-mode 'rust-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'rust-mode))

(use-package sh-script
  :defer t
  :config
  (evil-leader/set-key-for-mode 'sh-mode
    "mgg" #'dumb-jump-go)
  (which-key-declare-prefixes-for-mode 'sh-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'sh-mode))

(use-package typescript-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'typescript-mode
    "mgg" #'lsp-find-definition
    "mgd" #'lsp-find-declaration
    "mgi" #'lsp-find-implementation
    "mgt" #'lsp-find-type-definition)
  (which-key-declare-prefixes-for-mode 'typescript-mode
    (concat evil-leader/leader "mg") "goto")
  (evil-leader/set-major-leader-for-mode 'typescript-mode))

(use-package view
  :defer t
  :config
  (evil-set-initial-state 'view-mode 'normal)
  (add-hook 'view-mode-hook
            (lambda ()
              (ignore-errors
                (evil-local-set-key 'normal (kbd "q") #'quit-window)))))

(use-package vlf
  :defer t
  :config
  (add-hook 'vlf-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "M-p") #'vlf-prev-batch)
              (evil-local-set-key 'normal (kbd "M-n") #'vlf-next-batch))))

(use-package ztree-view
  :defer t
  :init
  (defn ztree-back-node (&optional node)
    (interactive)
    (let ((line (line-number-at-pos)))
      (-when-let (node (or node (ztree-find-node-in-line line)))
        (if (funcall ztree-node-is-expandable-fun node)
            (when (ztree-is-expanded-node node)
              (ztree-do-toggle-expand-state node nil))
          (setq line (ztree-get-parent-for-line line))
          (ztree-do-toggle-expand-state (ztree-find-node-in-line line) nil))
        (ztree-refresh-buffer line))))

  (defn ztree-enter-node (&optional node)
    (interactive)
    (let ((line (line-number-at-pos)))
      (-when-let (node (or node (ztree-find-node-in-line line)))
        (if (funcall ztree-node-is-expandable-fun node)
            (progn
              (ztree-do-toggle-expand-state node t)
              (ztree-refresh-buffer line))
          (when ztree-node-action-fun
            (funcall ztree-node-action-fun node t))))))

  :config
  (add-hook 'ztreediff-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "RET") #'ztree-perform-action)
              (evil-local-set-key 'normal (kbd "l") #'ztree-enter-node)
              (evil-local-set-key 'normal (kbd "h") #'ztree-back-node)
              (evil-local-set-key 'normal (kbd "q") #'kill-buffer))))

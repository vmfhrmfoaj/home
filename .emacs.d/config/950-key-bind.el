;; -*- lexical-binding: t; -*-

(eval-and-compile
  (eval-when-compile
    (unless (file-exists-p "~/.emacs.d/config/func.elc")
      (byte-compile-file "~/.emacs.d/config/func.el")))
  (require 'evil-core)
  (require 'evil-leader nil t)
  (load-file "~/.emacs.d/config/func.el"))


(global-set-key (kbd "<S-kp-multiply>") "#")
(global-set-key (kbd "<S-kp-divide>") "\\")
(global-set-key (kbd "<S-kp-subtract>") "_")
(global-set-key (kbd "<S-kp-add>") "=")
(define-key input-decode-map (kbd "<S-kp-multiply>") "#")
(define-key input-decode-map (kbd "<S-kp-divide>") "\\")
(define-key input-decode-map (kbd "<S-kp-subtract>") "_")
(define-key input-decode-map (kbd "<S-kp-add>") "=")

;; minibuffer
(define-key isearch-mode-map (kbd "<escape>") #'isearch-cancel)
(define-key isearch-mode-map (kbd "C-g") #'isearch-cancel)
(define-key isearch-mode-map (kbd "C-h")
  (lambda ()
    (interactive)
    (if (and isearch-success (not isearch-error))
        (isearch-delete-char)
      (while (or (not isearch-success) isearch-error) (isearch-pop-state))
      (isearch-update))))
(remove-hook 'minibuffer-setup-hook #'evil-initialize)
(when evil-want-minibuffer
  (evil-define-key 'normal minibuffer-local-map
    (kbd "<escape>") #'abort-recursive-edit
    (kbd "RET") #'exit-minibuffer)
  (evil-define-key 'insert minibuffer-local-map
    (kbd "RET") #'exit-minibuffer
    (kbd "<tab>") #'completion-at-point))
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when evil-want-minibuffer
              (evil-initialize)
              (set (make-local-variable 'evil-echo-state) nil)
              (evil-insert-state))
            (local-set-key (kbd "C-a") #'beginning-of-line)
            (local-set-key (kbd "C-b") #'backward-char)
            (local-set-key (kbd "C-h") #'backward-delete-char)))

(use-package bind-map
  :ensure t
  :after evil-leader
  :init
  (eval-when-compile (require 'bind-map nil t)))

(use-package evil-leader
  :ensure t
  :init
  (eval-when-compile (require 'evil-leader nil t))

  (defvar evil-leader--major-leader ","
    "TODO")

  :config
  (defun evil-leader--set-major-leader (key)
    (let ((old-m-leader evil-leader--major-leader))
      (setq evil-leader--major-leader key)
      (dolist (mode (-map #'car evil-leader--mode-maps))
        (evil-leader--set-major-leader-for-mode mode))))

  (defun evil-leader--set-major-leader-for-mode (mode)
    ;; for `evil-leader'
    (-when-let (map (-some->> evil-leader--mode-maps
                              (assoc mode)
                              (-drop 1)
                              (assoc 109) ; 109 = "m"
                              (-drop 1)))
      (let ((map-name (intern (format "evil-leader-for-%s-map" mode))))
        (eval
         `(progn
            (defvar ,map-name ',map)
            (bind-map ,map-name
              :evil-keys (,evil-leader--major-leader)
              :evil-states (normal)
              :major-modes (,mode))))))

    ;; for `which-key'
    (-when-let (it (assoc mode which-key-replacement-alist))
      (let* ((evil-leader (s-chop-prefix "<" (s-chop-suffix ">" evil-leader/leader)))
             (its-vals (cdr it))
             (its-new-vals
              (--map (let ((prefix (caar it))
                           (more (cdr it)))
                       (cons (list (s-replace (concat evil-leader " m") evil-leader--major-leader prefix)) more))
                     its-vals)))
        (setcdr it (append its-vals its-new-vals)))))

  (defun evil-leader--set-local-key (&rest bindings)
    (let* ((prefix (concat evil-leader/leader "m"))
           (bindings (->> bindings
                          (-partition 2)
                          (--map (cons (concat evil-leader/leader (car it)) (cdr it)))
                          (--mapcat (if (not (s-starts-with? prefix (car it)))
                                        (list it)
                                      (list it (cons (s-replace prefix evil-leader--major-leader (car it)) (cdr it))))))))
      (dolist (binding bindings)
        (evil-local-set-key 'normal (car binding) (cadr binding)))))

  (evil-leader/set-leader "<SPC>")
  (evil-leader--set-major-leader ",")
  (evil-leader/set-key
    "<SPC>" #'counsel-M-x
    "TAB" #'projectile-switch-to-previous-buffer
    "`" #'eshell
    "'" #'projectile-run-eshell
    "!" #'shell-command
    ";" #'comment-it
    "u" #'universal-argument

    ;; applications
    ;; - org
    "aoa" (defalias 'org-agenda-show-list
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window #'org-agenda-list #'org-agenda-goto-today)))
    "aocj" #'org-clock-goto
    "aocn" #'org-capture-note
    "aoct" #'org-capture-todo
    "aom" (defalias 'org-agenda-show-tags
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window #'org-tags-view)))
    "aoM" (defalias 'org-agenda-show-tags-todo-only
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window (-partial #'org-tags-view t))))
    "aos" (defalias 'org-agenda-show-search-result
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window #'org-search-view)))
    "aoS" (defalias 'org-agenda-show-search-result-todo-only
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window (-partial #'org-search-view t))))
    "aot" (defalias 'org-agenda-show-todo-list
            (lambda ()
              (interactive)
              (org-agenda-show-on-dedicated-window #'org-todo-list)))
    ;; - docker
    "ad" #'docker
    ;; - undo-tree
    "au" #'undo-tree-visualize

    ;; buffer
    "bR" #'revert-buffer
    "ba" #'counsel-switch-buffer
    "bb" #'counsel-projectile-switch-to-buffer
    "bd" #'kill-buffer-and-delete-window
    "be" #'eldoc-doc-buffer
    "bk" #'projectile-kill-buffer
    "bK" #'kill-buffer
    "bs" #'pop-to-scratch-buffer

    ;; error
    "en" #'flycheck-next-error
    "ep" #'flycheck-previous-error
    "el" #'counsel-flycheck
    "es" #'show-error-list

    ;; file
    "fb" #'counsel-bookmark
    "ff" #'counsel-find-file
    "fR" #'rename-current-buffer-file
    "fr" #'counsel-recentf
    "ft" #'treemacs-current-directory
    "fY" #'kill-new-buffer-file-name
    "fy" #'projectile-kill-new-buffer-file-name

    ;; git
    "g=" #'git-gutter:stage-hunk
    "gb" #'magit-blame-addition
    "gl" #'counsel-git-log
    "gs" #'magit-status
    "gt" #'git-timemachine
    "gp" #'git-gutter:previous-hunk
    "gn" #'git-gutter:next-hunk

    ;; jump/join/split
    "jn" #'newline-and-indent
    "jo" #'split-line
    "js" #'sp-split-sexp

    ;; narrow
    "nf" (defalias 'custom-narrow-to-defun
           (lambda ()
             (interactive)
             (-let (((beg . end) (bounds-of-thing-at-point 'defun)))
               (when (or (not beg) (not end))
                 (let ((pos (point))
                       (beg-of-defun-func (if (functionp beginning-of-defun-function)
                                              beginning-of-defun-function
                                            #'beginning-of-defun))
                       (end-of-defun-func (if (functionp end-of-defun-function)
                                              end-of-defun-function
                                            #'end-of-defun)))
                   (setq end (progn (funcall end-of-defun-func) (point)))
                   (while (not (< pos end))
                     (setq end (progn (funcall end-of-defun-func) (point))))
                   (setq end (line-beginning-position))
                   (setq beg (progn (funcall beg-of-defun-func) (point)))
                   (while (not (< beg pos))
                     (setq beg (progn (funcall beg-of-defun-func) (point))))
                   (setq beg (line-beginning-position))))
               (narrow-to-region beg end))))
    "np" #'narrow-to-page
    "nr" #'narrow-to-region
    "nw" #'widen

    ;; project
    "p TAB" #'projectile-switch-latest-open-project
    "p'" #'projectile-run-eshell
    "p!" #'projectile-run-shell-command-in-root
    "pA" #'projectile-add-known-project
    "pD" #'projectile-remove-known-project
    "pI" #'projectile-invalidate-cache
    "pS" #'projectile-switch-project
    "pa" #'projectile-add-buffer-to-project
    "pd" #'projectile-find-dir
    "pf" #'counsel-projectile-find-file
    "pk" #'projectile-kill-buffers
    "ps" #'projectile-custom-switch-open-project
    "pt" #'treemacs-projectile-current

    ;; register/rings/resume
    "ri" #'ivy-resume
    "rk" #'counsel-yank-pop
    "rl" #'ivy-resume-non-search
    "rm" #'counsel-evil-marks
    "rr" #'counsel-evil-registers
    "rs" #'ivy-resume-sarch

    ;; search
    "sf" (if (executable-find "rg")
             (defalias 'counsel-rg-on-cur-dir
               (lambda (&optional dir)
                 (interactive)
                 (when (<= 4 (prefix-numeric-value current-prefix-arg))
                   (counsel-read-directory-name "rg in directory: "))
                 (counsel-rg nil (or dir default-directory))))
           #'counsel-custom-grep)
    "sF" (if (executable-find "rg")
             (defalias 'counsel-rg-on-cur-dir-wo-ignore
               (lambda (&optional dir)
                 (interactive)
                 (when (<= 4 (prefix-numeric-value current-prefix-arg))
                   (counsel-read-directory-name "rg in directory: "))
                 (let ((counsel-rg-base-command (counsel-rg-no-ignore-command)))
                   (counsel-rg nil (or dir default-directory)))))
           #'counsel-custom-grep)
    "sp" (if (executable-find "rg")
             #'counsel-projectile-rg
           #'counsel-projectile-git-grep)
    "sP" (if (executable-find "rg")
             (defalias 'counsel-projectile-rg-wo-ignore
               (lambda ()
                 (interactive)
                 (let ((counsel-rg-base-command (counsel-rg-no-ignore-command)))
                   (counsel-projectile-rg))))
           #'counsel-projectile-grep)
    "ss" #'swiper

    ;; toggle
    "tl" #'toggle-truncate-lines
    "tm" #'toggle-frame-maximized
    "tw" #'whitespace-mode

    ;; quit
    "qq" #'save-buffers-kill-emacs

    ;; window
    "w-" #'split-window-vertically
    "w=" #'balance-windows
    "w\\" #'split-window-horizontally
    "wh" #'windmove-left
    "wj" #'windmove-down
    "wk" #'windmove-up
    "wl" #'windmove-right
    "wd" #'delete-window
    "wm" (defalias 'delete-up-or-down-windows
           (lambda ()
             (interactive)
             (-some-> 'up   (window-in-direction) (delete-window))
             (-some-> 'down (window-in-direction) (delete-window))))
    "wM" #'delete-other-windows
    "wo" (defalias 'move-to-main-frame
           (lambda ()
             (interactive)
             (let ((buf (current-buffer)))
               (switch-to-previous-buffer)
               (pop-to-buffer buf))))
    "wp" #'pin-window

    ;; text / xwidget
    "x0" (defalias 'text-scale-reset (lambda () (interactive) (text-scale-set 0)))
    "x=" #'text-scale-increase
    "x-" #'text-scale-decrease
    "xa" #'align-regexp
    "xb" #'xwidget-webkit-browse-url
    "xf" #'format-buffer-or-region
    "xo" #'open-link-at-point
    "xr" #'font-lock-update)
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
  :init
  (eval-when-compile (require 'which-key nil t))

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
    (concat evil-leader/leader "s") "search"
    (concat evil-leader/leader "t") "toggle"
    (concat evil-leader/leader "q") "quit"
    (concat evil-leader/leader "w") "window"
    (concat evil-leader/leader "x") "text/xwidget"))


;; Key binding for the minor mode

(use-package alchemist-help
  :disabled t
  :defer t
  :init
  (eval-when-compile (require 'alchemist-help nil t))

  :config
  (evil-define-key 'normal alchemist-help-minor-mode-map
    (kbd "q") #'kill-buffer-and-delete-window))

(use-package company
  :defer t
  :init
  (eval-when-compile (require 'company nil t))

  :config
  (define-key company-active-map (kbd "<tab>")     #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-active-map (kbd "C-j")
    (lambda ()
      (interactive)
      (if (ignore-errors
            (and company-tooltip-flip-when-above
                 (-some->> 'company-replacement-args
                           (overlay-get company-pseudo-tooltip-overlay)
                           (nth 3))))
          (call-interactively #'company-select-previous)
        (call-interactively #'company-select-next))))
  (define-key company-active-map (kbd "C-k")
    (lambda ()
      (interactive)
      (if (ignore-errors
            (and company-tooltip-flip-when-above
                 (-some->> 'company-replacement-args
                           (overlay-get company-pseudo-tooltip-overlay)
                           (nth 3))))
          (call-interactively #'company-select-next)
        (call-interactively #'company-select-previous))))
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-s") #'counsel-company)
  (define-key company-active-map (kbd "SPC") #'company-abort-and-insert-space)
  (define-key company-active-map (kbd "<S-return>") (lambda () (interactive) (company-cancel) (newline-and-indent)))
  (define-key company-active-map (kbd "<C-return>")
    (lambda () (interactive) (company-complete-selection) (evil-normal-state)))

  (add-hook 'company-mode-hook
            (lambda ()
              (evil-local-set-key 'insert (kbd "<tab>") #'company-indent-or-complete-common))))

(use-package cider-inspector
  :defer t
  :init
  (eval-when-compile (require 'cider-inspector-mode nil t))

  (evil-set-initial-state 'cider-inspector-mode 'normal)

  :config
  (evil-define-key 'normal cider-inspector-mode-map
    (kbd "q") #'quit-window))

(use-package cider-mode
  :defer t
  :init
  (eval-when-compile (require 'cider-mode nil t))

  (add-hook 'cider-mode-hook
            (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string pos)
                             (when (cider-connected-p)
                               (let ((buf (current-buffer)))
                                 (ignore-errors
                                   (call-interactively #'cider-find-var-at-point))
                                 (unless (and (eq buf (current-buffer))
                                              (= pos (point)))
                                   t))))))))

(use-package cider-repl
  :defer t
  :init
  (eval-when-compile (require 'cider-repl nil t))

  :config
  (define-key cider-repl-mode-map (kbd "C-l") #'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "RET") nil)

  (evil-define-key 'normal cider-repl-mode-map
    (kbd "RET") #'cider-repl-return)

  (evil-leader/set-key-for-mode 'cider-repl-mode
    "mee" #'cider-eval-last-sexp
    "mef" #'cider-eval-defun-at-point
    "mer" #'cider-eval-last-sexp-and-replace
    "mrc" #'cider-repl-clear-buffer
    "mrq" #'cider-quit
    "mrR" #'cider-hard-restart
    "mrr" #'cider-restart
    "msn" #'cider-repl-set-ns)
  (which-key-declare-prefixes-for-mode 'cider-repl-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "repl"
    (concat evil-leader/leader "ms") "set/change")
  (evil-leader--set-major-leader-for-mode 'cider-repl-mode)

  (let ((fn (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string _position)
                             (let ((buf (current-buffer))
                                   (pos (point)))
                               (call-interactively #'cider-find-var-at-point)
                               (unless (and (eq buf (current-buffer))
                                            (= pos (point)))
                                 t)))))))
    (add-hook 'cider-repl-mode-hook fn)))

(use-package cider-stacktrace
  :defer t
  :init
  (eval-when-compile (require 'cider-stacktrace nil t))

  :config
  (evil-define-key 'normal cider-stacktrace-mode-map
    (kbd "q") #'kill-buffer-and-delete-window))

(use-package dumb-jump
  :init
  (eval-when-compile (require 'dumb-jump nil t))

  (let ((fn (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string pos)
                             (let ((buf (current-buffer)))
                               (ignore-errors
                                 (call-interactively #'dumb-jump-go))
                               (unless (and (eq buf (current-buffer))
                                            (= pos (point)))
                                 t)))))))
    (add-hook 'prog-mode-hook fn)))

(use-package evil-collection-docker
  :ensure evil-collection
  :defer t
  :init
  (eval-when-compile (evil-collection-require 'docker t)))

(use-package evil-collection-eshell
  :ensure evil-collection
  :defer t
  :init
  (eval-when-compile (evil-collection-require 'eshell t)))

(use-package evil-collection-magit
  :ensure evil-collection
  :defer t
  :init
  (eval-when-compile (evil-collection-require 'magit t)))

(use-package evil
  :defer t
  :init
  (eval-when-compile (require 'evil nil t))

  :config
  (define-key evil-outer-text-objects-map "U" 'evil-a-sexp)
  (define-key evil-inner-text-objects-map "U" 'evil-inner-sexp)
  (evil-define-key 'normal 'global
    "gb" #'evil-multiedit-match-and-next
    "gB" #'evil-multiedit-match-all
    "gh" #'evil-lookup
    "gr" #'eldoc-refresh
    (kbd "<tab>") #'indent-for-tab-command
    (kbd "<C-backspace>") #'evil-backward-word-begin
    (kbd "C-d") (lambda () (interactive) (call-interactively #'up-list))
    (kbd "C-u") (lambda () (interactive) (call-interactively #'backward-up-list))
    (kbd "C-h c") #'describe-char
    (kbd "C-s-SPC") #'mark-sexp
    (kbd "C-k") #'evil-scroll-page-up
    (kbd "C-j") #'evil-scroll-page-down)
  (evil-define-key 'insert 'global
    (kbd "<C-return>") #'evil-normal-state ; for company compatibility
    (kbd "C-d") (lambda () (interactive) (call-interactively #'up-list))
    (kbd "C-u") (lambda () (interactive) (call-interactively #'backward-up-list))
    (kbd "C-h") #'backward-delete-char
    (kbd "C-a") #'beginning-of-line-text
    (kbd "C-e") #'end-of-line
    (kbd "C-r") #'eldoc-refresh
    (kbd "C-v") #'yank
    (kbd "C-j") nil
    (kbd "C-k") nil)
  (evil-define-key 'visual 'global
    (kbd "<tab>") #'indent-region
    (kbd "v") #'er/expand-region))

(use-package evil-ex
  :defer t
  :init
  (eval-when-compile (require 'evil-ex nil t))

  :config
  (define-key evil-ex-completion-map [remap abort-recursive-edit] #'abort-recursive-edit-for-evil-ex)
  (define-key evil-ex-completion-map (kbd "M-p") #'previous-history-element)
  (define-key evil-ex-completion-map (kbd "M-n") #'next-history-element)
  (when evil-want-minibuffer
    (evil-define-key 'normal evil-ex-completion-map
      (kbd "<escape>") #'abort-recursive-edit-for-evil-ex)
    (evil-define-key 'insert evil-ex-completion-map
      (kbd "<escape>") (lambda ()
                         (interactive)
                         (if evil-ex-expression
                             (evil-normal-state)
                           (abort-recursive-edit-for-evil-ex))))))

(use-package evil-multiedit
  :defer t
  :init
  (eval-when-compile (require 'evil-multiedit nil t))

  :config
  (bind-keys
   :map evil-multiedit-state-map
   ("p" . (lambda ()
            (interactive)
            (when-let ((it (current-kill 0)))
              (iedit-replace-occurrences it))))
   ("n" . evil-multiedit-next)
   ("N" . evil-multiedit-prev)
   ([tab] . (lambda ()
              (interactive)
              (iedit-toggle-selection)
              (evil-multiedit-next))))
  (let ((fn (lambda ()
              (interactive)
              (iedit-toggle-selection)
              (evil-multiedit-prev))))
    (define-key evil-multiedit-state-map [backtab] fn)
    (define-key evil-multiedit-state-map [S-tab] fn)
    (define-key evil-multiedit-state-map [S-iso-lefttab] fn)))

(use-package evil-org
  :defer t
  :init
  (eval-when-compile (require 'evil-org nil t))

  :config
  (evil-org-set-key-theme))

(use-package evil-org-agenda
  :ensure evil-org
  :after org-agenda
  :init
  (eval-when-compile (require 'evil-org-agenda nil t))

  :config
  (evil-org-agenda-set-keys)
  (define-key org-agenda-mode-map "q" nil)
  (define-key org-agenda-mode-map "Q" nil)
  (define-key org-agenda-mode-map "x" nil))

(use-package evil-surround
  :defer t
  :init
  (eval-when-compile (require 'evil-surround nil t))

  :config
  (evil-define-key 'visual evil-surround-mode-map
    "S" #'evil-substitute
    "s" #'evil-surround-region))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile (require 'flycheck nil t))

  :config
  (evil-define-key 'normal flycheck-error-list-mode-map
    (kbd "RET") #'flycheck-error-list-goto-error
    "n" #'flycheck-error-list-next-error
    "p" #'flycheck-error-list-previous-error
    "q" #'quit-window))

(use-package git-timemachine
  :defer t
  :init
  (eval-when-compile (require 'git-timemachine nil t))

  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    (kbd "i")   #'evil-normal-state
    (kbd "M-p") #'git-timemachine-show-previous-revision
    (kbd "M-n") #'git-timemachine-show-next-revision
    (kbd "M-b") #'git-timemachine-blame
    (kbd "M-s") #'git-timemachine-show-commit
    (kbd "M-w") #'git-timemachine-kill-abbreviated-revision
    (kbd "M-W") #'git-timemachine-kill-revision
    (kbd "q")   #'git-timemachine-quit))

(use-package ivy
  :defer t
  :init
  (eval-when-compile (require 'ivy nil t))

  (evil-set-initial-state 'ivy-occur-mode      'normal)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

  :config
  (if (not evil-want-minibuffer)
      (progn
        (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-partial)
        (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
        (define-key ivy-minibuffer-map (kbd "C-u") #'ivy-parent-dir))

    (defun ivy--open-it-other-window-and-exit ()
      (interactive)
      (let ((caller (ivy-state-caller ivy-last)))
        (cond
         ((memq caller
                '(counsel-projectile-switch-to-buffer
                  ivy-switch-buffer))
          (ivy-exit-with-action #'ivy--switch-buffer-other-window-action))
         ((memq caller
                '(counsel-projectile-find-file
                  counsel-find-file))
          (ivy-exit-with-action (lambda (file)
                                  (find-file-other-window (if (zerop (length file))
                                                              ivy-text
                                                            file))))))))

    (evil-define-key 'insert ivy-minibuffer-map
      (kbd "<tab>") #'ivy-partial
      (kbd "<C-return>") #'ivy-immediate-done
      (kbd "<M-return>") #'ivy--open-it-other-window-and-exit
      (kbd "C-,") #'ivy-minibuffer-shrink
      (kbd "C-.") #'ivy-minibuffer-grow
      (kbd "C-f") (lambda ()
                    (interactive)
                    (evil-normal-state)
                    (ivy-toggle-calling))
      (kbd "C-j") #'ivy-next-line
      (kbd "C-k") #'ivy-previous-line
      (kbd "C-u") #'ivy-parent-dir)

    (evil-define-key 'normal ivy-minibuffer-map
      (kbd "RET") #'ivy-done
      (kbd "<C-return>") #'ivy-immediate-done
      (kbd "<M-return>") #'ivy--open-it-other-window-and-exit
      "j" #'ivy-next-line
      "k" #'ivy-previous-line
      (kbd "C-,") #'ivy-minibuffer-shrink
      (kbd "C-.") #'ivy-minibuffer-grow
      (kbd "C-f") #'ivy-toggle-calling
      (kbd "C-j") #'ivy-next-line
      (kbd "C-k") #'ivy-previous-line
      (kbd "C-u") (lambda ()
                    (interactive)
                    (when (ivy-parent-dir)
                      (evil-insert-state)))
      (kbd "<tab>") (lambda ()
                      (interactive)
                      (let ((input ivy-text))
                        (ivy-partial)
                        (unless (string= input ivy-text)
                          (evil-insert-state))))))

  (cl-letf (((symbol-function 'display-warning) #'ignore))
    (evil-collection-require 'ivy)
    (evil-collection-ivy-setup)))

(use-package ivy-hydra
  :ensure t
  :defer t
  :commands (ivy-minibuffer-shrink
             ivy-minibuffer-grow)
  :init
  (eval-when-compile (require 'ivy-hydra nil t)))

(use-package lsp-mode
  :defer t
  :init
  (eval-when-compile (require 'lsp-mode nil t))

  :config
  (defvar lsp--custom-setup-key-status '())

  (defun lsp--custom-setup-key ()
    "Set up keys for `lsp-mode'."
    (unless (member major-mode lsp--custom-setup-key-status)
      (evil-leader/set-key-for-mode major-mode
        ;; sessions
        "msr" #'lsp-workspace-restart
        "mss" #'lsp
        "msq" #'lsp-workspace-shutdown
        "msd" #'lsp-describe-session
        "msD" #'lsp-disconnect

        ;; folders
        "mFa" #'lsp-workspace-folders-add
        "mFr" #'lsp-workspace-folders-remove
        "mFb" #'lsp-workspace-blacklist-remove

        ;; toggles
        "mTf" (if (lsp-feature? "textDocument/onTypeFormatting") #'lsp-toggle-on-type-formatting)
        "mTT" #'lsp-treemacs-sync-mode

        ;; goto
        "mgr" (if (lsp-feature? "textDocument/references") #'lsp-find-references)
        "mgi" (if (lsp-feature? "textDocument/implementation") #'lsp-find-implementation)
        "mgt" (if (lsp-feature? "textDocument/typeDefinition") #'lsp-find-type-definition)
        "mgd" (if (lsp-feature? "textDocument/declaration") #'lsp-find-declaration)
        "mgh" (if (and (lsp-feature? "callHierarchy/incomingCalls")
                       (fboundp 'lsp-treemacs-call-hierarchy))
                  #'lsp-treemacs-call-hierarchy)
        "mga" (if (lsp-feature? "workspace/symbol") #'xref-find-apropos)
        "mgs" (cond
               ((and (lsp-feature? "textDocument/documentSymbol")
                     (not (or (derived-mode-p 'clojure-mode)
                              (derived-mode-p 'php-mode)
                              (derived-mode-p 'python-mode))))
                #'lsp-ivy-doc-symbol)
               ((lsp-feature? "workspace/symbol")
                #'lsp-ivy-workspace-symbol-for-cur-file))
        "mgS" (if (and (fboundp 'lsp-ivy-global-workspace-symbol)
                       (lsp-feature? "workspace/symbol"))
                  #'lsp-ivy-workspace-symbol)

        ;; help
        "mhh" (if (lsp-feature? "textDocument/hover") #'lsp-describe-thing-at-point)
        "mhs" (if (lsp-feature? "textDocument/signatureHelp") #'lsp-signature-activate)
        "mhg" (if (and (featurep 'lsp-ui-doc)
                       (lsp-feature? "textDocument/hover"))
                  #'lsp-ui-doc-glance)

        ;; refactoring
        "mrr" (if (lsp-feature? "textDocument/rename") #'lsp-rename)
        "mro" (if (lsp-feature? "textDocument/rename") #'lsp-organize-imports)

        ;; actions
        "maa" (if (lsp-feature? "textDocument/codeAction") #'lsp-execute-code-action)
        "mah" (if (lsp-feature? "textDocument/documentHighlight") #'lsp-document-highlight))
      (which-key-declare-prefixes-for-mode major-mode
        (concat evil-leader/leader "ms")  "sessions"
        (concat evil-leader/leader "mF")  "folders"
        (concat evil-leader/leader "m=")  "formatting"
        (concat evil-leader/leader "mT")  "toggle"
        (concat evil-leader/leader "mg")  "goto"
        (concat evil-leader/leader "mh")  "help"
        (concat evil-leader/leader "mr")  "refactor / repl"
        (concat evil-leader/leader "ma")  "code actions")
      (evil-leader--set-major-leader-for-mode major-mode)
      (add-to-list 'lsp--custom-setup-key-status major-mode)))

  (add-hook 'lsp-after-open-hook
            (lambda ()
              (lsp--custom-setup-key)
              (make-local-variable 'evil-goto-definition-functions)
              (when-let ((fn (cond
                              ;; NOTE
                              ;;  `Perl-LanguageServer` is hanging when requrest `textDocument/definition`.
                              ((derived-mode-p 'cperl-mode 'perl-mode)
                               nil)
                              ((lsp-feature? "textDocument/definition")
                               (lambda (_string pos)
                                 (let ((buf (current-buffer)))
                                   (ignore-errors
                                     (call-interactively #'lsp-find-definition))
                                   (unless (and (eq buf (current-buffer))
                                                (= pos (point)))
                                     t)))))))
                (add-to-list 'evil-goto-definition-functions fn)))))

(use-package magit-blame
  :defer t
  :init
  (eval-when-compile (require 'magit-blame nil t))

  :config
  (--each '("p" "P" "n" "N" "b" "r" "f" "B")
    (when-let ((f (lookup-key magit-blame-read-only-mode-map (kbd it))))
      (evil-define-key* 'normal magit-blame-mode-map (kbd (concat "M-" it)) f))
    (define-key magit-blame-read-only-mode-map (kbd it) nil)))

(use-package smartparens
  :defer t
  :init
  (eval-when-compile (require 'smartparens nil t))

  :config
  (evil-leader/set-key
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
    "kw" #'sp-wrap-sexp)

  (evil-define-key 'normal smartparens-mode-map
    "%" #'sp--simulate-evil-jump-item))

(use-package treemacs-evil
  :defer t
  :init
  (eval-when-compile (require 'treemacs-evil nil t))

  :config
  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") #'treemacs-close-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-open-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "q") #'treemacs-kill-buffer))


;; Key binding for the major mode

(use-package clojure-mode
  :defer t
  :init
  (eval-when-compile (require 'clojure-mode nil t))

  :config
  (dolist (mode '(clojure-mode clojurec-mode clojurescript-mode))
    (evil-leader/set-key-for-mode mode
      "meb" #'cider-eval-buffer
      "mee" #'cider-eval-last-sexp
      "mef" #'cider-eval-defun-at-point
      "mel" #'cider-inspect-last-result
      "men" #'cider-eval-ns-form
      "mer" #'cider-eval-last-sexp-and-replace
      "mr'" #'cider-switch-to-releated-repl-buffer
      "mri" #'cider-insert-last-sexp-in-repl
      "mrq" #'cider-quit)
    (which-key-declare-prefixes-for-mode mode
      (concat evil-leader/leader "me") "evaluation"
      ;;  already defined at `lsp-mode'
      ;; (concat evil-leader/leader "mr") "repl"
      )
    (evil-leader--set-major-leader-for-mode mode)))

(use-package cperl-mode
  :defer t
  :init
  (eval-when-compile (require 'cperl-mode nil t))

  :config
  (define-key cperl-mode-map (kbd "{") nil) ; disable `cperl-electric-lbrace'
  (evil-define-key 'normal cperl-mode-map
    (kbd "M-,") #'pop-tag-mark))

(use-package dired
  :config
  :init
  (eval-when-compile (require 'dired nil t))

  (defun dired-alternate-up-directory ()
    "TODO"
    (interactive)
    (let* ((dir (dired-current-directory))
	       (up (file-name-directory (directory-file-name dir))))
      (find-alternate-file up)))

  (evil-define-key 'normal dired-mode-map (kbd "C-u") #'dired-alternate-up-directory))

(use-package docker
  :defer t
  :init
  (eval-when-compile (require 'docker nil t))

  (let ((hooks '(docker-container-mode-hook
                 docker-image-mode-hook
                 docker-machine-mode-hook
                 docker-network-mode-hook
                 docker-volume-mode-hook)))
    (defun docker-setup-once-for-evil-keybinding ()
      (dolist (hook hooks)
        (remove-hook hook #'docker-setup-once-for-evil-keybinding))
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (evil-collection-require 'docker))

      (evil-collection-docker-setup)
      (dolist (map evil-collection-docker-maps)
        (evil-collection-define-key 'normal map "q" #'kill-buffer-and-delete-window))

      (fmakunbound #'docker-setup-once-for-evil-keybinding))

    (dolist (hook hooks)
      (add-hook hook #'docker-setup-once-for-evil-keybinding))))

(use-package doc-view
  :defer t
  :init
  (eval-when-compile (require 'doc-view nil t))

  :config
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-define-key 'normal doc-view-mode-map
    "+" #'doc-view-enlarge
    "=" #'doc-view-enlarge
    "0" #'doc-view-scale-reset
    "-" #'doc-view-shrink
    "gg" #'doc-view-first-page
    (kbd "M-p") #'doc-view-previous-page
    "k" #'doc-view-previous-line-or-previous-page
    "j" #'doc-view-next-line-or-next-page
    (kbd "M-n") #'doc-view-next-page
    "G" #'doc-view-last-page
    "q" #'kill-buffer-and-delete-window))

(use-package ediff
  :defer t
  :init
  (eval-when-compile (require 'ediff nil t))

  :config
  (advice-add #'ediff-setup-keymap :after
              (lambda ()
                (define-key ediff-mode-map (kbd "J") #'ediff-jump-to-difference)
                (define-key ediff-mode-map (kbd "k") #'ediff-previous-difference)
                (define-key ediff-mode-map (kbd "j") #'ediff-next-difference)
                (define-key ediff-mode-map (kbd "0") #'ediff-reset-text-size)
                (define-key ediff-mode-map (kbd "+") #'ediff-increase-text-size)
                (define-key ediff-mode-map (kbd "-") #'ediff-decrease-text-size))))

(use-package elisp-mode
  :defer t
  :init
  (eval-when-compile (require 'elisp-mode nil t))

  :config
  ;; emacs-lisp-mode
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "mee" #'eval-last-sexp
    "mef" #'eval-defun
    "mr'" #'emacs-lisp-REPL-buffer)
  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "repl")
  (evil-leader--set-major-leader-for-mode 'emacs-lisp-mode)

  ;; lisp-interaction-mode
  (evil-leader/set-key-for-mode 'lisp-interaction-mode
    "mee" #'emacs-lisp-REPL-eval-print-this-sexp
    "mrc" #'erase-buffer
    "mrq" #'evil-delete-buffer)
  (which-key-declare-prefixes-for-mode 'lisp-interaction-mode
    (concat evil-leader/leader "me") "evaluation"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mr") "repl")
  (evil-leader--set-major-leader-for-mode 'lisp-interaction-mode)

  (let ((fn (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string _position)
                             (let ((buf (current-buffer))
                                   (pos (point)))
                               (call-interactively #'elisp-slime-nav-find-elisp-thing-at-point)
                               (unless (and (eq buf (current-buffer))
                                            (= pos (point)))
                                 t)))))))
    (add-hook 'emacs-lisp-mode-hook       fn)
    (add-hook 'lisp-interaction-mode-hook fn)))

(use-package elixir-mode
  :disabled t
  :defer t
  :init
  (eval-when-compile (require 'elixir-mode nil t))

  :config
  (let ((fn (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string _position)
                             (let ((buf (current-buffer))
                                   (pos (point)))
                               (call-interactively #'dumb-jump-go)
                               (unless (and (eq buf (current-buffer))
                                            (= pos (point)))
                                 t)))))))
    (add-hook 'elixir-mode-hook fn)))

(use-package elm-mode
  :disabled t
  :defer t
  :init
  (eval-when-compile (require 'elm-mode nil t))

  :config
  (let ((fn (lambda ()
              (make-local-variable 'evil-goto-definition-functions)
              (add-to-list 'evil-goto-definition-functions
                           (lambda (_string _position)
                             (let ((buf (current-buffer))
                                   (pos (point)))
                               (call-interactively #'dumb-jump-go)
                               (unless (and (eq buf (current-buffer))
                                            (= pos (point)))
                                 t)))))))
    (add-hook 'elm-mode-hook fn)))

(use-package eshell
  :defer t
  :init
  (eval-when-compile (require 'eshell nil t))

  (evil-set-initial-state 'eshell-mode 'insert)

  (defun eshell-setup-once-for-evil-keybinding ()
    (remove-hook 'eshell-mode-hook #'eshell-setup-once-for-evil-keybinding)
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (evil-collection-require 'eshell))

    (evil-collection-eshell-setup)
    (let ((f (lambda ()
               (evil-collection-eshell-setup-keys)
               (evil-collection-define-key 'normal 'eshell-mode-map "I"
                 (lambda ()
                   (interactive)
                   (eshell-bol)
                   (evil-insert-state)))
               (evil-collection-define-key 'insert 'eshell-mode-map (kbd "C-l") #'eshell/clear)
               (evil-collection-define-key 'insert 'eshell-mode-map (kbd "<tab>") #'company-complete-common))))
      (add-hook 'eshell-mode-hook f)
      (funcall f))

    (fmakunbound #'eshell-setup-once-for-evil-keybinding))

  (add-hook 'eshell-mode-hook #'eshell-setup-once-for-evil-keybinding))

(use-package help-mode
  :defer t
  :init
  (eval-when-compile (require 'help-mode nil t))

  :config
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key 'normal help-mode-map
    (kbd "<backtab>") #'backward-button
    (kbd "<tab>") #'forward-button
    (kbd "M-,") #'help-go-back
    (kbd "q") #'quit-window))

(use-package js
  :defer t
  :init
  (eval-when-compile (require 'js nil t))

  :config
  (evil-leader/set-key-for-mode 'js-mode
    "m=!" #'web-beautify-js))

(use-package markdown-mode
  :defer t
  :init
  (eval-when-compile (require 'markdown-mode nil t))

  :config
  (evil-leader/set-key-for-mode 'markdown-mode
    "msp" #'markdown-preview
    "mso" #'markdown-open-link-at-point)
  (which-key-declare-prefixes-for-mode 'markdown-mode
    (concat evil-leader/leader "ms") "Show")
  (evil-leader--set-major-leader-for-mode 'markdown-mode))

(use-package magit
  :defer t
  :init
  (eval-when-compile (require 'magit nil t))

  (evil-set-initial-state 'magit-status-mode 'normal)

  (defun magit-setup-once-for-evil-keybinding ()
    (remove-hook 'magit-blame-mode-hook #'magit-setup-once-for-evil-keybinding)
    (remove-hook 'magit-mode-hook       #'magit-setup-once-for-evil-keybinding)
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (evil-collection-require 'magit))

    (evil-collection-magit-setup)

    (fmakunbound #'magit-setup-once-for-evil-keybinding))

  (add-hook 'magit-blame-mode-hook #'magit-setup-once-for-evil-keybinding)
  (add-hook 'magit-mode-hook       #'magit-setup-once-for-evil-keybinding)

  :config
  (define-key transient-base-map (kbd "C-g")      #'transient-quit-all)
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one)
  (define-key transient-map (kbd "C-g")      #'transient-quit-all)
  (define-key transient-map (kbd "<escape>") #'transient-quit-one))

(use-package magit-svn
  :after (magit evil-collection-magit)
  :init
  (eval-when-compile (require 'magit-svn nil t))

  :config
  (evil-collection-define-key 'normal 'magit-mode-map (kbd "~") #'magit-svn))

(use-package multi-term
  :defer t
  :init
  (eval-when-compile (require 'multi-term nil t))

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
  :init
  (eval-when-compile (require 'org nil t))

  :config
  (evil-leader/set-key-for-mode 'org-mode
    "m:" #'counsel-org-tag
    "mTT" #'org-todo
    "mci" #'org-clock-in
    "mco" #'org-clock-out
    "mcj" #'org-clock-goto
    "mgs" #'counsel-org-goto
    "mgS" #'counsel-org-goto-all
    "mih" #'org-insert-heading
    "miH" #'org-insert-subheading
    "mtI" #'org-time-stamp-inactive
    "mtd" #'org-deadline
    "mti" #'org-time-stamp
    "mts" #'org-schedule)
  (which-key-declare-prefixes-for-mode 'org-mode
    (concat evil-leader/leader "mT") "todo"
    (concat evil-leader/leader "mc") "clock"
    (concat evil-leader/leader "mg") "goto"
    (concat evil-leader/leader "mi") "insert"
    (concat evil-leader/leader "mt") "time")
  (evil-leader--set-major-leader-for-mode 'org-mode)
  (evil-define-key 'normal evil-org-mode-map
    (kbd "RET") #'org-open-at-point
    (kbd "M-,") #'org-mark-ring-goto))

(use-package package
  :defer t
  :init
  (eval-when-compile (require 'package nil t))

  :config
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-define-key 'normal package-menu-mode-map
    (kbd "U") #'package-menu-mark-upgrades
    (kbd "d") #'package-menu-mark-delete
    (kbd "i") #'package-menu-mark-install
    (kbd "q") #'kill-buffer-and-delete-window
    (kbd "x") #'package-menu-execute))

(use-package php-mode
  :defer t
  :init
  (eval-when-compile (require 'php-mode nil t))

  :config
  (evil-leader/set-key-for-mode 'php-mode
    "mr\"" #'psysh-restart
    "mr'" #'psysh-show)
  ;; NOTE:
  ;;  already defined at `lsp-mode'
  ;; (which-key-declare-prefixes-for-mode 'python-mode
  ;;   (concat evil-leader/leader "mr") "repl")
  (define-key php-mode-map [tab] nil))

(use-package profiler
  :defer t
  :init
  (eval-when-compile (require 'profiler nil t))

  :config
  (evil-define-key 'normal profiler-report-mode-map
    (kbd "<tab>") #'profiler-report-expand-entry))

(use-package psysh
  :defer t
  :init
  (eval-when-compile (require 'psysh nil t))

  :config
  (evil-define-key 'normal 'psysh-mode-map
    (kbd "M-p") #'comint-previous-input
    (kbd "M-n") #'comint-next-input))

(use-package python
  :defer t
  :init
  (eval-when-compile (require 'python nil t))

  :config
  (evil-leader/set-key-for-mode 'python-mode
    "mr'" #'run-python)
  ;; NOTE:
  ;;  already defined at `lsp-mode'
  ;; (which-key-declare-prefixes-for-mode 'python-mode
  ;;   (concat evil-leader/leader "mr") "repl")
  )

(use-package view
  :defer t
  :init
  (eval-when-compile (require 'view nil t))

  :config
  (evil-set-initial-state 'view-mode 'normal)
  (add-hook 'view-mode-hook
            (lambda ()
              (ignore-errors
                (evil-local-set-key 'normal (kbd "q") #'quit-window)))))

(use-package vlf
  :defer t
  :init
  (eval-when-compile (require 'vlf nil t))

  :config
  (add-hook 'vlf-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "M-p") #'vlf-prev-batch)
              (evil-local-set-key 'normal (kbd "M-n") #'vlf-next-batch)
              (evil-local-set-key 'normal (kbd "gg") #'vlf-custom-beginning-of-file)
              (evil-local-set-key 'normal (kbd "G")  #'vlf-custom-end-of-file))))

(use-package winum
  :defer t
  :config
  (with-eval-after-load "magit"
    (define-key magit-section-mode-map (kbd "M-1") nil)
    (define-key magit-section-mode-map (kbd "M-2") nil)
    (define-key magit-section-mode-map (kbd "M-3") nil)
    (define-key magit-section-mode-map (kbd "M-4") nil))

  (evil-global-set-key 'normal (kbd "M-0") #'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-1") #'winum-select-window-1)
  (evil-global-set-key 'normal (kbd "M-2") #'winum-select-window-2)
  (evil-global-set-key 'normal (kbd "M-3") #'winum-select-window-3)
  (evil-global-set-key 'normal (kbd "M-4") #'winum-select-window-4)
  (evil-global-set-key 'normal (kbd "M-5") #'winum-select-window-5)
  (evil-global-set-key 'normal (kbd "M-6") #'winum-select-window-6)
  (evil-global-set-key 'normal (kbd "M-7") #'winum-select-window-7)
  (evil-global-set-key 'normal (kbd "M-8") #'winum-select-window-8)
  (evil-global-set-key 'normal (kbd "M-9") #'winum-select-window-9)
  (evil-global-set-key 'normal (kbd "M-0") #'winum-select-window-0)

  (evil-global-set-key 'insert (kbd "M-0") (lambda () (interactive) (evil-normal-state) (winum-select-window-0)))
  (evil-global-set-key 'insert (kbd "M-1") (lambda () (interactive) (evil-normal-state) (winum-select-window-1)))
  (evil-global-set-key 'insert (kbd "M-2") (lambda () (interactive) (evil-normal-state) (winum-select-window-2)))
  (evil-global-set-key 'insert (kbd "M-3") (lambda () (interactive) (evil-normal-state) (winum-select-window-3)))
  (evil-global-set-key 'insert (kbd "M-4") (lambda () (interactive) (evil-normal-state) (winum-select-window-4)))
  (evil-global-set-key 'insert (kbd "M-5") (lambda () (interactive) (evil-normal-state) (winum-select-window-5)))
  (evil-global-set-key 'insert (kbd "M-6") (lambda () (interactive) (evil-normal-state) (winum-select-window-6)))
  (evil-global-set-key 'insert (kbd "M-7") (lambda () (interactive) (evil-normal-state) (winum-select-window-7)))
  (evil-global-set-key 'insert (kbd "M-8") (lambda () (interactive) (evil-normal-state) (winum-select-window-8)))
  (evil-global-set-key 'insert (kbd "M-9") (lambda () (interactive) (evil-normal-state) (winum-select-window-9)))
  (evil-global-set-key 'insert (kbd "M-0") (lambda () (interactive) (evil-normal-state) (winum-select-window-0)))

  (global-set-key (kbd "M-1") #'winum-select-window-1)
  (global-set-key (kbd "M-2") #'winum-select-window-2)
  (global-set-key (kbd "M-3") #'winum-select-window-3)
  (global-set-key (kbd "M-4") #'winum-select-window-4)
  (global-set-key (kbd "M-5") #'winum-select-window-5)
  (global-set-key (kbd "M-6") #'winum-select-window-6)
  (global-set-key (kbd "M-7") #'winum-select-window-7)
  (global-set-key (kbd "M-8") #'winum-select-window-8)
  (global-set-key (kbd "M-9") #'winum-select-window-9))

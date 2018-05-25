(use-package helm-swoop
  :ensure t
  :defer t
  :commands (helm-swoop)
  :init
  (defun helm-swoop--get-content-for-fancy-narrow (buf &optional linum)
    (let* (ret
           (buf (or (get-buffer buf) (current-buffer)))
           ;; NOTE:
           ;;  a advice of the compiled fundamental functions is not working.
           ;;  see http://nullprogram.com/blog/2013/01/22/
           (pos-min (or fancy-narrow--beginning (point-min)))
           (pos-max (or fancy-narrow--end (point-max)))
           (str (with-current-buffer buf
                  (buffer-substring-no-properties pos-min pos-max)))
           (num (line-number-at-pos pos-min))
           (fmt (concat "%-"
                        (-> pos-max
                            (line-number-at-pos)
                            (number-to-string)
                            (length)
                            (number-to-string))
                        "s "))
           (colorize (byte-compile (lambda (it) (propertize it 'font-lock-face 'helm-swoop-line-number-face))))
           (insert-linum (-compose #'insert
                                   (if helm-swoop-use-line-number-face
                                       colorize
                                     #'identity)
                                   (-partial #'format fmt))))
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (funcall insert-linum num)
        (while (progn (forward-line) (not (eobp)))
          (-update-> num (1+))
          (if (and (not linum)
                   (looking-at-p "^[0-9]+\\s-*$"))
              (kill-line)
            (funcall insert-linum num)))
        (setq ret (buffer-substring (point-min) (point-max))))
      ret))

  :config
  (setq helm-swoop-pre-input-function (-const "")
        helm-swoop-speed-or-color nil
        helm-swoop-split-window-function #'helm-default-display-buffer
        helm-swoop-use-line-number-face t)
  (advice-add #'helm-swoop--get-content :override #'helm-swoop--get-content-for-fancy-narrow))

(use-package helm-ag
  :ensure t
  :defer t
  :commands (helm-do-ag
             helm-do-ag-project-root)

  :config
  (setq helm-ag-base-command "rg --no-heading"
	      helm-ag-use-emacs-lisp-regexp t))

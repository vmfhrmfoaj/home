(defun helm-company-action-insert-plus (candidate)
  "Insert CANDIDATE."
  (let* ((company-candidates (helm-attr 'company-candidates))
         (company-backend (helm-attr 'company-backend))
         (selection (cl-find-if (byte-compile (lambda (s) (string-match-p candidate s))) company-candidates))
         (company-common (helm-attr 'company-common))
         (company-prefix company-common))
    (company-finish selection))
  ;; for GC
  (helm-attrset 'company-candidates nil))

(defun helm-company-plus ()
  (interactive)
  (unless company-candidates
    (company-complete))
  (when company-point
    (company-complete-common)
    (let* ((src    (copy-alist helm-source-company))
           (action (--remove (string-equal "Insert" (car it)) helm-company-actions)))
      (add-to-list 'action '("Insert" . helm-company-action-insert-plus))
      (setf (alist-get 'action src) action)
      (helm :sources src
            :input (concat company-common " ")
            :buffer  "*helm company*"
            :candidate-number-limit helm-company-candidate-number-limit))))

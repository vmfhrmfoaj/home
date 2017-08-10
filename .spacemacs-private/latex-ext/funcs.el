(defun build-tex-file (&optional file)
  (interactive)
  (let ((file (or file buffer-file-name)))
    (when (and (file-exists-p file)
               (= 0 (call-process "pdflatex" nil nil nil file)))
      (concat (file-name-sans-extension file) ".pdf"))))

(defun correct-foregound-color (png)
  (let ((fg-color (face-attribute 'default :foreground))
        (bg-color (face-attribute 'default :background)))
    (unless (equal (color-values "white")
                   (color-values bg-color))
      ;; XXX: brew install graphicsmagick
      (shell-command-to-string (concat "gm convert "
                                       png
                                       " -fill '" fg-color "'"
                                       " -opaque black"
                                       " " png)))))

(defun doc-view-pdf->png-converter-ghostscript-wrapper (pdf png page callback)
  (let ((cb (lexical-let ((callback callback)
                          (png      png))
              (lambda ()
                (if (file-exists-p png)
                    (correct-foregound-color png)
                  (--map (correct-foregound-color it)
                         (-> png
                             file-name-directory
                             (directory-files t ".+\\.png")))
                  (unless (bound-and-true-p doc-view-refresh-once)
                    (setq-local doc-view-refresh-once t)
                    (run-at-time "1 secs" nil
                                 #'doc-view-revert-buffer nil t)))
                (funcall callback)))))
    (doc-view-pdf->png-converter-ghostscript pdf
                                             png
                                             page
                                             cb)))

;;; packages.el --- latex-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinseop Kim <vmfhrmfoaj@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `latex-ext-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `latex-ext/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `latex-ext/pre-init-PACKAGE' and/or
;;   `latex-ext/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst latex-ext-packages
  '(auctex
    (doc-view :location built-in)))

(defun latex-ext/post-init-auctex ()
  (use-package tex
    :defer t
    :config
    (byte-compile #'build-tex-file)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (-when-let (pdf (build-tex-file))
                  (save-window-excursion
                    (if (->> (frame-parameters)
                             (assoc 'fullscreen)
                             cdr)
                        (find-file-other-window pdf)
                      (find-file-other-frame pdf))))
                (add-hook 'after-save-hook
                          (lambda (&rest _)
                            (message "Build & Reload PDF file")
                            ;; TODO:
                            ;; execute asynchronously
                            (-when-let (pdf (build-tex-file))
                              (-when-let (buf (->> (frame-list)
                                                   (--mapcat (window-list it))
                                                   (--map (window-buffer it))
                                                   (--filter (string-equal pdf (buffer-file-name it)))
                                                   (-first-item)))
                                (save-window-excursion
                                  (set-buffer buf)
                                  (doc-view-revert-buffer nil t)))))
                          nil t)))))

(defun latex-ext/post-init-doc-view ()
  (use-package doc-view
    :defer t
    :config
    (byte-compile #'correct-foregound-color)
    (byte-compile #'doc-view-pdf->png-converter-ghostscript-wrapper)
    (setq doc-view-ghostscript-options
          (->> doc-view-ghostscript-options
               (--remove (string-match-p "-sDEVICE=.*" it))
               (append '("-sDEVICE=pngalpha"))))
    (setq-default doc-view-pdf->png-converter-function
                  #'doc-view-pdf->png-converter-ghostscript-wrapper)
    (add-hook 'doc-view-mode-hook
              (lambda ()
                (global-hl-line-mode 0)
                (linum-mode 0)))))

;;; packages.el ends here

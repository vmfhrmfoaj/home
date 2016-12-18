;;; packages.el --- html-ext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: KimJinseop <Jinseop@KimJinseops-iMac.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst html-ext-packages
  '(css-mode
    web-mode))

(defun html-ext/post-init-css-mode ()
  (use-package css-mode
    :defer t
    :config
    (setq-default css-indent-offset 2)
    (setq css-color-distnat-foreground
          (let ((base (/ (color-distance "white" "black") 2))
                (bg (face-attribute 'default :background)))
            (if (> base (color-distance "white" bg))
                "white"
              "black")))
    (font-lock-add-keywords
     'css-mode
     `((,(concat "rgba?("
                 "\\([0-9A-Fa-f]+\\),[ \r\t\n]*"
                 "\\([0-9A-Fa-f]+\\),[ \r\t\n]*"
                 "\\([0-9A-Fa-f]+\\)\\(?:,[ \r\n\t]*0?.[0-9]+\\)?)")
        0 (put-text-property
           (match-beginning 0)
           (match-end 0)
           'face (list :distant-foreground css-color-distnat-foreground
                       :background (let ((max 255.0)
                                         (r (match-string 1))
                                         (g (match-string 2))
                                         (b (match-string 3)))
                                     (->> (list r g b)
                                          (-map #'string-to-int)
                                          (--map (min max it))
                                          (--map (/ it max))
                                          (apply #'color-rgb-to-hex))))))
       ;; http://ergoemacs.org/emacs/emacs_CSS_colors.html
       ("#[0-9A-Fa-f]\\{3,6\\}"
        0 (put-text-property
           (match-beginning 0)
           (match-end 0)
           'face (list :distant-foreground css-color-distnat-foreground
                       :background (let ((color (string-to-list (match-string 0))))
                                     (if (= 7 (length color))
                                         (apply #'string color)
                                       (->> color
                                            (--remove-first (char-equal ?# it))
                                            (-take 3)
                                            (--map (make-string 2 it))
                                            (apply #'concat "#")))))))))))

(defun html-ext/post-init-web-mode ()
  (use-package web-mode
    :defer t
    :config
    (setq-default web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))

;;; packages.el ends here

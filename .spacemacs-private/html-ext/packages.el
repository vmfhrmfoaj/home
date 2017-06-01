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
    (font-lock-add-keywords
     'css-mode
     `((,(concat "rgba?("
                 "\\([0-9A-Fa-f]+\\),[ \r\t\n]*"
                 "\\([0-9A-Fa-f]+\\),[ \r\t\n]*"
                 "\\([0-9A-Fa-f]+\\)\\(?:,[ \r\n\t]*0?.[0-9]+\\)?")
        0 (put-text-property
           (match-beginning 1)
           (match-end 3)
           'face (let* ((max 255.0)
                        (r   (match-string 1))
                        (g   (match-string 2))
                        (b   (match-string 3))
                        (bg  (->> (list r g b)
                                  (-map #'string-to-int)
                                  (--map (min max it))
                                  (--map (/ it max))
                                  (apply #'color-rgb-to-hex)))
                        (fg (light-color bg 40))
                        (fg (if (< 1000 (color-distance fg bg))
                                fg
                              (dim-color bg 40)))
                        (default-bg (face-attribute 'default :background)))
                   (if (> (min (color-distance default-bg "#d5d5d5")
                               (color-distance default-bg "#555555"))
                          (color-distance bg default-bg))
                       `(:foreground ,bg)
                     `(:inverse-video t :background ,fg :foreground ,bg)))))
       ;; http://ergoemacs.org/emacs/emacs_CSS_colors.html
       ("#[0-9A-Fa-f]\\{3,6\\}"
        0 (put-text-property
           (match-beginning 0)
           (match-end 0)
           'face (let* ((max   255.0)
                        (color (string-to-list (match-string 0)))
                        (bg    (if (= 7 (length color))
                                   (apply #'string color)
                                 (->> color
                                      (--remove-first (char-equal ?# it))
                                      (-take 3)
                                      (--map (make-string 2 it))
                                      (apply #'concat "#"))))
                        (fg (light-color bg 40))
                        (fg (if (< 1000 (color-distance fg bg))
                                fg
                              (dim-color bg 40)))
                        (default-bg (face-attribute 'default :background)))
                   (if (> (min (color-distance default-bg "#d5d5d5")
                               (color-distance default-bg "#555555"))
                          (color-distance bg default-bg))
                       `(:background ,bg)
                     `(:inverse-video t :foreground ,bg :background ,fg)))))))))

(defun html-ext/post-init-web-mode ()
  (use-package web-mode
    :defer t
    :config
    (setq-default web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))

;;; packages.el ends here

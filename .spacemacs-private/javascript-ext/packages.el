;;; packages.el --- %LAYER_NAME% layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: %USER_FULL_NAME% <%USER_MAIL_ADDRESS%>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst javascript-ext-packages
  '(js2-mode
    web-beautify))

(defun javascript-ext/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (defconst js--prettify-symbols-alist nil)
    (setq-default js-indent-level 2
                  js2-basic-offset 2)))

(defun javascript-ext/post-init-web-beautify ()
  (use-package web-beautify
    :defer t
    :config
    (-update->> web-beautify-args (append '("-s" "2")))))

;;; packages.el ends here

(defface perl-sepcial-variable-name-face
  `((t (:inherit font-lock-variable-name-face :weight normal)))
  "Face used to font-lock CPerl special variable.")

(defvar perl-indent-config--default
  '(var
    ((indent-tabs-mode nil))

    offset
    ((cperl-indent-level 2)
     (cperl-brace-offset 0)
     (cperl-continued-brace-offset -2)
     (cperl-label-offset -2)
     (cperl-continued-statement-offset 2)
     (cperl-extra-newline-before-brace nil)
     (cperl-extra-newline-before-brace-multiline nil)
     (cperl-merge-trailing-else t))))

(defcustom perl-indent-config
  perl-indent-config--default
  "TODO"
  :type 'list
  :safe 'listp)

;; TODO
;; Create custom `c-style'.
(defvar c-indent-config--default
  '(offset
    ((access-label          -1)
     (brace-list-intro      2)
     (case-label            1)
     (defun-block-intro     2)
     (inclass               2)
     (inextern-lang         0)
     (innamespace           0)
     (label                 0)
     (member-init-intro     2)
     (statement-block-intro 2)
     (statement-case-intro  1)
     (statement-case-intro  2)
     (statement-case-open   0)
     (statement-cont        2)
     (substatement          2)
     (substatement-open     0))))

;; TODO
;; Create custom `c-style'.
(defvar c-indent-config--antlabs
  '(var
    ((indent-tabs-mode t)
     (c-special-indent-hook nil))

    offset
    ((access-label          nil)
     (arglist-close         +)
     (arglist-intro         +)
     (brace-list-intro      +)
     (brace-list-open       0)
     (case-label            0)
     (defun-block-intro     +)
     (inclass               +)
     (inextern-lang         0)
     (innamespace           0)
     (label                 nil)
     (member-init-intro     +)
     (statement-block-intro +)
     (statement-case-intro  +)
     (statement-case-intro  +)
     (statement-case-open   0)
     (statement-cont        +)
     (substatement          +)
     (substatement-open     0)
     (substatement-label    0))))

(defcustom c-indent-config
  c-indent-config--default
  "TODO"
  :type 'list
  :safe 'listp)

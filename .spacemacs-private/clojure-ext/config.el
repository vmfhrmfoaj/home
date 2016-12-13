(defface clojure-side-effect-face
  `((t :foreground ,(face-attribute 'font-lock-warning-face :foreground)
       :slant italic
       :weight bold))
  "Face used to font-lock Clojure side-effect indicator.")

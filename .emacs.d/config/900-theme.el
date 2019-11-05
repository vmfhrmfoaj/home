(custom-set-faces
 '(diff-added          ((((class color) (background light)) :background "#cceecc" :foreground "#22aa22")))
 '(diff-changed        ((((class color) (background light)) :background "#eeeecc" :foreground "#aaaa22")))
 '(diff-removed        ((((class color) (background light)) :background "#eecccc" :foreground "#aa2222")))
 '(diff-refine-added   ((((class color) (background light)) :background "#ddffdd" :foreground "#119911")))
 '(diff-refine-changed ((((class color) (background light)) :background "#ffffdd" :foreground "#999911")))
 '(diff-refine-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#991111")))
 '(region ((((class color) (background light)) :background "light sky blue"))))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (custom-set-faces
   '(auto-dim-other-buffers-face ((((class color) (background light)) :background "#F3F3F3")))))

(use-package helm
  :defer t
  :config
  (custom-set-faces
   '(helm-match ((t :inherit lazy-highlight)))
   '(helm-match-item ((t :inherit lazy-highlight)))
   '(helm-match-selection ((t :inherit isearch)))))

(use-package powerline
  :defer t
  :config
  (custom-set-faces
   '(mode-line-inactive ((((class color) (background light))
                          :background "grey90"
                          :foreground "grey40"
                          :weight light
                          :box (:line-width -1 :color "grey75" :style nil))))))

(use-package magit
  :defer t
  :config
  (custom-set-faces
   '(magit-diff-context-highlight ((((class color) (background light)) :background "#FBFEEE" :foreground "#A4C207")))
   '(magit-section-highlight ((((class color) (background light)) :inherit hl-line :distant-foreground "black")))))

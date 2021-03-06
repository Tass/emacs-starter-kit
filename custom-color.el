(setq evil-default-cursor t)

(require 'color-theme)
(defun custom-color ()
  (interactive)
  (color-theme-install
   '(custom-color
     ((background-color . "#060001")
      (cursor-color . "#7eff00")
      (foreground-color . "#7eff00")
      (background-mode . dark)
      (border-color . "#323232")
      (mouse-color . "#323232"))
     (mode-line ((t (:foreground "#7eff00" :background "#323232"))))
     (region ((t (:background "#520000" ))))

     (font-lock-comment-face ((t (:foreground "#0784C4"))))
     (font-lock-constant-face ((t (:foreground "#D62DFF"))))
     (font-lock-builtin-face ((t (:foreground "#AA858C"))))
     (font-lock-function-name-face ((t (:foreground "#58A30C"))))
     (font-lock-variable-name-face ((t (:foreground "#7175A8"))))
     (font-lock-keyword-face ((t (:foreground "#9267C2"))))
     (font-lock-string-face ((t (:foreground "#D6199F"))))
     (font-lock-doc-string-face ((t (:foreground "#D6199F"))))
     (font-lock-type-face ((t (:foreground "#A89E00"))))
     )))

(custom-color)
(provide 'custom-color)

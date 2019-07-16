(require 'scicpp-common)

(deftheme scicpp-light
  "Modified spacemacs light theme.")

(setq spacemacs-theme-custom-colors
      '(
        (str . "#aa0000")
        (act1 . "#888888")
        (act2 . "#999999")
        (lnum . "#999999")
        (highlight . "#cccccc")
        (green-bg-s . "#cccccc") ; for lazy highlight
        (bg1 . "#ffffff")
        (keyword . "#2200dd")
        (const . "#000000")
        (type . "#000044")
        (var . "#000000")
        (func . "#0000aa")
        (base . "#101010")
        (base-dim . "#121212")
        (comment . "#006600")
        ))

(scicpp-init-pre)
(create-spacemacs-theme 'light 'scicpp-light)
(scicpp-init-post)

(custom-set-faces
 '(column-enforce-face ((t (:background "#cccccc"))))
 '(centaur-tabs-default ((t (:background "#aaaaaa" :foreground "#333333"))))
 '(centaur-tabs-selected ((t (:background "#cccccc" :foreground "black" :box nil))))
 '(centaur-tabs-unselected ((t (:background "#888888" :foreground "#333333" :box nil))))
 '(centaur-tabs-selected-modified ((t (:background "#ffcccc" :foreground "#333333" :box nil))))
 '(centaur-tabs-unselected-modified ((t (:background "#ffaaaa" :foreground "#333333" :box nil))))
 '(centaur-tabs-active-bar-face ((t (:background "#C9D9FF" :box nil))))
 '(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#c9d9ff" :box nil))))
 '(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#c9d9ff" :box nil))))
 )

(provide-theme 'scicpp-light)
;;; scicpp-light ends here

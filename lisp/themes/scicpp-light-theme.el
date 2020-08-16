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
 '(centaur-tabs-default ((t (:background "#aaaaaa" :foreground "#333333"))))
 '(centaur-tabs-selected ((t (:background "#cccccc" :foreground "black" :box nil))))
 '(centaur-tabs-unselected ((t (:background "#888888" :foreground "#333333" :box nil))))
 '(centaur-tabs-selected-modified ((t (:background "#ffcccc" :foreground "#333333" :box nil))))
 '(centaur-tabs-unselected-modified ((t (:background "#ffaaaa" :foreground "#333333" :box nil))))
 '(centaur-tabs-active-bar-face ((t (:background "#C9D9FF" :box nil))))
 '(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#c9d9ff" :box nil))))
 '(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#c9d9ff" :box nil))))
 ;; sunrise commander
 '(sunrise-marked-file-face ((t :foreground "orange")))
 '(sunrise-highlight-path-face ((t :background nil :foreground "#ace6ac" :bold t :height 120)))
 '(sunrise-active-path-face ((((type tty) (class color) (min-colors 8))
                              :background nil :foreground "orange" :bold t)
                             (((type tty) (class mono)) :inverse-video t)
                             (t :background nil :foreground "orange" :bold t :height 120)))
 '(sunrise-passive-path-face ((((type tty) (class color) (min-colors 8) (background dark))
                               :background nil :foreground "cyan")
                              (((type tty) (class color) (min-colors 8) (background light))
                               :background nil :foreground "cyan")
                              (t :background nil :foreground "gray" :bold t :height 120)))
 )

(provide-theme 'scicpp-light)
;;; scicpp-light ends here

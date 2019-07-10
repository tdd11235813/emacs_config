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

(custom-set-faces '(column-enforce-face ((t (:background "#ffffff")))))

(provide-theme 'scicpp-light)
;;; scicpp-light ends here

(require 'scicpp-common)

(deftheme scicpp-dark
  "Modified spacemacs dark theme.")

(setq spacemacs-theme-custom-colors
      '(
        (str . "#ff8866")
        (act1 . "#303030")
        (act2 . "#404040")
        (lnum . "#999999")
        (highlight . "#444444")
        (green-bg-s . "#444444") ; for lazy highlight
        (bg1 . "#262626")
        (keyword . "#99cc55")
        (const . "#ffffff")
        (type . "#88ee88")
        (var . "#aaffaa") ;; currently not overwritten by spacemacs-common.el (#149)
        (func . "#ff99ff")
        (base . "#b2b2b2")
        (base-dim . "#888888")
        (comment . "#5f8787")
        ))

(scicpp-init-pre)
(create-spacemacs-theme 'dark 'scicpp-dark)
(scicpp-init-post)

(custom-set-faces '(column-enforce-face ((t (:background "#161616")))))


(provide-theme 'scicpp-dark)
;;; scicpp-dark ends here

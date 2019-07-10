(require 'spacemacs-common)

(deftheme test-space
  "Modified spacemacs theme.")

(custom-set-variables '(spacemacs-theme-comment-bg . nil))
(custom-set-variables '(spacemacs-theme-custom-colors
      '(
        (str . "#ff0000")
        (act1 . "#ff0000")
        (act2 . "#ff0000")
        (lnum . "#ff0000")
        (highlight . "#ff0000")
        (highlight-dim . "#ff0000")
        (err . "#ff0000")
        (war . "#ff0000")
        (green-bg-s . "#ff0000")
        (bg1 . "#ff0000")
        (keyword . "#ff0000")
        (const . "#ff0000")
        (type . "#ff0000")
        (var . "#ff0000")
        (func . "#ff0000")
        (base . "#ff0000")
        (base-dim . "#ff0000")
        (comment . "#ff0000")
        )))

(create-spacemacs-theme 'dark 'test-space)

(provide-theme 'test-space)

(require 'spacemacs-common)

(deftheme test-space-light
  "Modified spacemacs theme.")

(setq spacemacs-theme-comment-bg nil)
(setq spacemacs-theme-custom-colors
      '(
        (str . "#0000ff")
        (act1 . "#0000ff")
        (act2 . "#0000ff")
        (lnum . "#0000ff")
        (highlight . "#0000ff")
        (highlight-dim . "#0000ff")
        (err . "#0000ff")
        (war . "#0000ff")
        (green-bg-s . "#0000ff")
        (bg1 . "#0000ff")
        (keyword . "#0000ff")
        (const . "#0000ff")
        (type . "#0000ff")
        (var . "#0000ff")
        (func . "#0000ff")
        (base . "#0000ff")
        (base-dim . "#0000ff")
        (comment . "#0000ff")
        ))

(create-spacemacs-theme 'dark 'test-space-light)

(provide-theme 'test-space-light)

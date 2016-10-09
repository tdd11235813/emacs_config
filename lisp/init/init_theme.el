;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:

(defun init-theme-dark (alternative)
  "Initialize dark theme."

  ;; (defvar zenburn-override-colors-alist
  ;;   '(("zenburn-bg" . "#282828")
  ;;     ("zenburn-bg-1" . "#101010")
  ;;      ))
  ;;(load-theme 'zenburn t)

  (use-package spacemacs-common
    :ensure spacemacs-theme
    :init
    (show-paren-mode t)
    (add-hook 'prog-mode-hook 'show-paren-mode t)
    (use-package linum
      :config
      (setq linum-format "%4d ")
      )
    (setq lazy-highlight-cleanup nil)

    (setq spacemacs-theme-org-highlight t)
    (setq spacemacs-theme-org-height nil)
    (custom-set-variables '(spacemacs-theme-comment-bg . nil))
    (custom-set-variables '(spacemacs-theme-custom-colors
                            '((str . "#ff8866")
                              (act1 . (if (eq alternative '1) "#000030" "#303030"))
                              (act2 . (if (eq alternative '1) "#111111" "#404040"))
                              (lnum . "#f0f000")
                              (highlight . "#444444")
                              (green-bg-s . "#444444") ; for lazy highlight
                              (bg1 . (if (eq alternative '1) "#202020" "#262626"))
                              (keyword . (if (eq alternative '1) "#22ddff" "#99cc55"))
                              (const . "#ffffff")
                              (type . "#88ee88")
                              (var . "#aaffaa")
                              (func . "#ff99ff")
                              (base . (if (eq alternative '1) "#cccccc" "#b2b2b2"))
                              (base-dim . (if (eq alternative '1) "#999999" "#888888"))
                              (comment . (if (eq alternative '1) "#666666" "#5f8787"))
                              )))
    :config
    (use-package column-enforce-mode
      :config
      (setq column-enforce-comments nil)
      (custom-set-faces '(column-enforce-face ((t (:background "#161616")))))
      )
    (use-package highlight-numbers)
    (use-package rainbow-delimiters)
    (load-theme 'spacemacs-dark t)
    (set-face-attribute font-lock-keyword-face nil :weight 'normal :underline nil)
    (defface c-func-highlight-face '((t (:inherit bold :foreground "#87afaf")))
      "func() highlighting.")
    (defface cpp-arg-highlight-face '((t (:foreground "#aabbcc")))
      "Highlighting of _xxx vars.")
    (defface cpp-member-highlight-face '((t (:foreground "#dedede")))
      "Highlighting of xxx_ vars.")
    (defface cpp-member-access-highlight-face '((t (:foreground "#eeeeee")))
      "Highlighting of (::|.|->)vars.")

    (defvar c-mode-add-keywords '(("( *\\<\\([A-Za-z0-9_]+\\) *\\*" 1 'default) ; fix W was recognized as [pointer] type in ( W * xxx )
                                  ("\\<\\(if\\|for\\|else\\|switch\\|while\\|catch\\) *(" 1 'font-lock-keyword-face) ; exclude control statements
                                  ("\\<\\([a-zA-Z_]+[a-zA-Z0-9_]*\\) *\\(< *[a-zA-Z_,]+ *>\\)? *(" 1 'c-func-highlight-face) ; func name [<template>] (
                                  ("\\<\\(T[a-zA-Z_]*\\)\\> *\\([^(]\\{1\\}\\)"  1 'font-lock-type-face)
                                  ("\\<\\(_[a-z]+[a-zA-Z_]*\\)\\>"  1 'cpp-arg-highlight-face)
                                  ("\\<\\([a-z]+[a-zA-Z_]*_\\)\\>"  1 'cpp-member-highlight-face)
                                  ("\\(\\[[a-zA-Z0-9_]+\\]\\)?\\(\\.\\|->\\|::\\)\\{1\\}\\<\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)\\>"  3 'cpp-member-access-highlight-face) ; [..](.|->|::)
                                  ))
    (font-lock-add-keywords 'c-mode c-mode-add-keywords)
    (font-lock-add-keywords 'c++-mode c-mode-add-keywords)
    (add-hook 'prog-mode-hook 'column-enforce-mode)
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

    (use-package glsl-mode
      :load-path "lisp/"
      :config
      (font-lock-add-keywords 'glsl-mode c-mode-add-keywords)
      (add-hook 'glsl-mode-hook 'column-enforce-mode)
      (add-hook 'glsl-mode-hook 'highlight-numbers-mode)
      (add-hook 'glsl-mode-hook #'rainbow-delimiters-mode)
      )
    (use-package cuda-mode
      :config
      (font-lock-add-keywords 'cuda-mode c-mode-add-keywords)
      (add-hook 'cuda-mode-hook 'column-enforce-mode)
      ;; (add-hook 'cuda-mode-hook 'highlight-numbers-mode) ; conflicts too (write 'unsigned const' in func sign -> it hangs up)
      ;; (add-hook 'cuda-mode-hook #'rainbow-delimiters-mode) ; same
      )

    (use-package spaceline-config
      :ensure spaceline
      :config
      (spaceline-spacemacs-theme)
      (use-package semantic/sb
		   :ensure f)
      )
    )
  )

(provide 'init_theme)

;;; init_theme ends here

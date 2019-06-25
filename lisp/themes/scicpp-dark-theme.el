(deftheme scicpp-dark
  "Modified spacemacs dark theme.")


(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  (custom-set-variables '(spacemacs-theme-comment-bg . nil))
  ;; https://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-1-8
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '(
                            (str . "#ff8866")
                            (act1 . "#000030")
                            (act2 . "#111111")
                            (lnum . "#999999")
                            (highlight . "#444444")
                            (green-bg-s . "#444444") ; for lazy highlight
                            (bg1 . "#202020")
                            (keyword . "#22ddff")
                            (const . "#ffffff")
                            (type . "#88ee88")
                            (var . "#aaffaa")
                            (func . "#ff99ff")
                            (base . "#cccccc")
                            (base-dim . "#999999")
                            (comment . "#666666")
                            )))

  :config
  (create-spacemacs-theme 'dark 'scicpp-dark)
  (setq spacemacs-theme-org-highlight t)
  (setq spacemacs-theme-org-height nil)

  (use-package column-enforce-mode
    :delight column-enforce-mode
    :config
    (setq column-enforce-comments nil)
    (custom-set-faces '(column-enforce-face ((t (:background "#161616")))))
    )

  (use-package spaceline-config
    :ensure spaceline
    :defer 1
    :config
    (spaceline-spacemacs-theme)
    (spaceline-toggle-version-control-on)
    (setq-default
     powerline-height 24
     powerline-default-separator 'wave
     spaceline-flycheck-bullet "❖ %s"
     spaceline-separator-dir-left '(right . right)
     spaceline-separator-dir-right '(left . left))
    ;; (use-package semantic/sb
    ;;  :ensure f)
    )

  (show-paren-mode t)
  (add-hook 'prog-mode-hook 'show-paren-mode t)
  ;; line numbers
  (when (< emacs-major-version 26)
    (use-package linum
      :config
      (setq linum-format "%4d ")
      ))
  (when (>= emacs-major-version 26)
    (use-package display-line-numbers
      :disabled
      :defer 1
      :ensure f
      :config
      ;;(global-display-line-numbers-mode)
      ;; (customize-set-variable 'display-line-numbers-grow-only t) ;; do not shrink again
      (customize-set-variable 'display-line-numbers-width-start 3)
      ;; Disable line-numbers minor mode for neotree
      ;; https://github.crookster.org/macOS-Emacs-26-display-line-numbers-and-me/#display-line-numbers
      (add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))
      )
    )

  (setq lazy-highlight-cleanup nil)

  (use-package glsl-mode
    :load-path "lisp/glsl-mode"
    :ensure f
    )
  (use-package cuda-mode
    :load-path "lisp/cuda-mode"
    :ensure f
    )

  (set-face-attribute font-lock-keyword-face nil :weight 'normal :underline nil)
  (defface c-func-highlight-face '((((background light)) (:inherit bold :foreground "#006a6a"))
                                   (t (:inherit bold :foreground "#87afaf")))
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
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

  (font-lock-add-keywords 'glsl-mode c-mode-add-keywords)
  (add-hook 'glsl-mode-hook
            (lambda () (run-hooks 'prog-mode-hook)))

  (font-lock-add-keywords 'cuda-mode c-mode-add-keywords)
  (add-hook 'cuda-mode-hook 'column-enforce-mode)
  (add-hook 'cuda-mode-hook 'highlight-numbers-mode) ; conflicts too (write 'unsigned const' in func sign -> it hangs up)
  (add-hook 'cuda-mode-hook #'rainbow-delimiters-mode) ; same


  (use-package highlight-numbers)
  (use-package rainbow-delimiters)

  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (flycheck-mode -1)
    )
  )

(provide-theme 'scicpp-dark)

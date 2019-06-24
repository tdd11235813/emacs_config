;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:

(defun init-spacetheme (alternative)
  (use-package spacemacs-common
    :ensure spacemacs-theme
    :init

    (setq spacemacs-theme-org-highlight t)
    (setq spacemacs-theme-org-height nil)
    (custom-set-variables '(spacemacs-theme-comment-bg . nil))
    (custom-set-variables '(spacemacs-theme-custom-colors
                            '(
                              (concat str (when (= alternative 2) "#aa0000" "#ff8866"))
                              (concat act1 (when (= alternative 2) "#888888" (when (= alternative 1) "#000030" "#303030")))
                              (concat act2 (when (= alternative 2) "#999999" (when (= alternative 1) "#111111" "#404040")))
                              (concat lnum "#999999")
                              (concat highlight (when (= alternative 2) "#cccccc" "#444444"))
                              (concat green-bg-s (when (= alternative 2) "#cccccc" "#444444")) ; for lazy highlight
                              (concat bg1 (when (= alternative 2) "#ffffff" (when (= alternative 1) "#202020" "#262626")))
                              (concat keyword (when (= alternative 2) "#2200dd" (when (= alternative 1) "#22ddff" "#99cc55")))
                              (concat const (when (= alternative 2) "#000000" "#ffffff"))
                              (concat type (when (= alternative 2) "#000044" "#88ee88"))
                              (concat var (when (= alternative 2) "#000000" "#aaffaa"))
                              (concat func (when (= alternative 2) "#0000aa" "#ff99ff"))
                              (concat base (when (= alternative 2) "#101010" (when (= alternative 1) "#cccccc" "#b2b2b2")))
                              (concat base-dim (when (= alternative 2) "#121212" (when (= alternative 1) "#999999" "#888888")))
                              (concat comment (when (= alternative 2) "#006600" (when (= alternative 1) "#666666" "#5f8787")))
                              )))
    :config
    (use-package column-enforce-mode
      :delight column-enforce-mode
      :config
      (setq column-enforce-comments nil)
      (custom-set-faces '(column-enforce-face ((t (:background "#161616")))))
      )
    (load-theme 'spacemacs-dark t)

    (set-face-attribute font-lock-keyword-face nil :weight 'normal :underline nil)
    (defface c-func-highlight-face '((((background light)) (:inherit bold :foreground "#006a6a"))
                                     (t (:inherit bold :foreground "#87afaf")))
      "func() highlighting.")
    (defface cpp-arg-highlight-face '((t (if (eq alternative '2) (:foreground "#000000") (:foreground "#aabbcc"))))
      "Highlighting of _xxx vars.")
    (defface cpp-member-highlight-face '((t (if (eq alternative '2) (:foreground "#000000") (:foreground "#dedede"))))
      "Highlighting of xxx_ vars.")
    (defface cpp-member-access-highlight-face '((t (if (eq alternative '2) (:foreground "#000000") (:foreground "#eeeeee"))))
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
    ;; (add-hook 'cuda-mode-hook 'highlight-numbers-mode) ; conflicts too (write 'unsigned const' in func sign -> it hangs up)
    ;; (add-hook 'cuda-mode-hook #'rainbow-delimiters-mode) ; same

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

    )

  )

(defun init-theme-dark (alternative)
  "Initialize dark theme."
  (setq init-theme-alternative alternative)

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

  ;; spacemacs dark
  (init-spacetheme alternative)


  (use-package highlight-numbers)
  (use-package rainbow-delimiters)

  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (flycheck-mode -1)
    )
  )

(defun init-theme-light ()
  "Initialize light theme."
  ;; (interactive)
  ;; (disable-theme 'spacemacs-dark)

  (setq column-enforce-comments nil)
  (setq column-enforce-column 999)
  (global-column-enforce-mode nil)
  (setq init-theme-alternative-orig init-theme-alternative)
  (setq init-theme-alternative 2)
  (setq alternative 2)

  (set-background-color "#ffffff")
  (set-foreground-color "#000000")
  (custom-set-faces '(column-enforce-face ((t (:background "#ffffff")))))
  (load-theme 'spacemacs-light t)

  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (flycheck-mode -1)
    )
  )

(defun switch-theme-lightness ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq init-theme-alternative '2)
      (init-theme-dark init-theme-alternative-orig)
    (init-theme-light)
    )
  )

(provide 'init_theme)

;;; init_theme ends here

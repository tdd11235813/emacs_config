;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Inspired by http://aaronbedra.com/emacs.d/
;;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
;;; Code:

;; C-SPC SPC - set marker
;; C-u SPC - jump back
;; C-x C-x - select marked region, rectangle mode: C-x SPC

;;; Org
;; Alt+Enter, Alt+Shift+Right Subitem, Alt+Shift+Up, [Shift]-Tab Folding
;; C-c C-c Actions
;; C-c [ -> org.agenda

;;;
;; M-/ dabbrev-expand
;; M-m start of code line
;; M-<PageDwn> scroll other window
;; C-M-s regexp search
;; C-f expand-region
;; M-{ insert as pair
;; M-r line to bottom top
;; C-M <up/down> move level of parentheses
;; C-M-<prior|next> scroll-down|up-line

;; M-\ delete whitespaces
;; M-^ join to previous line
;; C-M-x eval defun (lisp)

;;; hs-minor-mode
;; C-c T h hs-minor-mode
;; C-c h a ha/hs-hide-all
;; C-c h s ha/hs-show-all
;; C-c h h ha/hs-toggle-hiding

;;(add-to-list 'load-path "~/.emacs.d/tmp/benchmark-init-el/")
;;(require 'benchmark-init-loaddefs)
;;(benchmark-init/activate)


(setq max-lisp-eval-depth 1000)
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq package-enable-at-startup nil)
(package-initialize)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))

(setq use-package-verbose nil)
;; if you need to install packages on the fly
;; (setq use-package-always-ensure t)

(use-package init_base
  :load-path "lisp/init/"
  :ensure f
  )

(use-package init_cpp
 :ensure f
  :load-path "lisp/init/"
  )

(use-package init_theme
  :ensure f
  :load-path "lisp/init/"
  ;; :bind
  ;; (
  ;;  ("C-c M-d" . init-theme-dark)
  ;;  ("C-c M-l" . init-theme-light)
  ;;  )
  ;; :config
  ;; (init-theme-dark 0)
  )
(init-theme-dark 0)

;;
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(cmake-ide-flags-c "-std=c++11")
 '(cmake-ide-flags-c++ "-std=c++11")
 '(company-clang-arguments (quote ("--gcc-toolchain=/sw/global/compilers/gcc/5.3.0")))
 '(flycheck-c/c++-clang-executable
   "/sw/global/compilers/llvm/3.7/bin/clang++ --gcc-toolchain=/sw/global/compilers/gcc/5.3.0")
 '(gc-cons-threshold 50000000)
 '(gnutls-min-prime-bits 2048)
 '(package-selected-packages
   (quote
    (php-mode spaceline rainbow-delimiters highlight-numbers column-enforce-mode spacemacs-theme glsl-mode modern-cpp-font-lock cmake-ide rtags cmake-mode auto-complete-clang flycheck cuda-mode drag-stuff misc-cmds yasnippet ws-butler ido-vertical-mode ido-hacks markdown-mode company sr-speedbar deft f use-package)))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-custom-colors
   (quote
    ((str if
          (eq alternative
              (quote 2))
          "#aa0000" "#ff8866")
     (act1 if
           (eq alternative
               (quote 2))
           "#888888"
           (if
               (eq alternative
                   (quote 1))
               "#000030" "#303030"))
     (act2 if
           (eq alternative
               (quote 2))
           "#999999"
           (if
               (eq alternative
                   (quote 1))
               "#111111" "#404040"))
     (lnum . "#f0f000")
     (highlight if
                (eq alternative
                    (quote 2))
                "#cccccc" "#444444")
     (green-bg-s if
                 (eq alternative
                     (quote 2))
                 "#cccccc" "#444444")
     (bg1 if
          (eq alternative
              (quote 2))
          "#ffffff"
          (if
              (eq alternative
                  (quote 1))
              "#202020" "#262626"))
     (keyword if
              (eq alternative
                  (quote 2))
              "#2200dd"
              (if
                  (eq alternative
                      (quote 1))
                  "#22ddff" "#99cc55"))
     (const if
            (eq alternative
                (quote 2))
            "#000000" "#ffffff")
     (type if
           (eq alternative
               (quote 2))
           "#000044" "#88ee88")
     (var if
          (eq alternative
              (quote 2))
          "#000000" "#aaffaa")
     (func if
           (eq alternative
               (quote 2))
           "#0000aa" "#ff99ff")
     (base if
           (eq alternative
               (quote 2))
           "#101010"
           (if
               (eq alternative
                   (quote 1))
               "#cccccc" "#b2b2b2"))
     (base-dim if
               (eq alternative
                   (quote 2))
               "#121212"
               (if
                   (eq alternative
                       (quote 1))
                   "#999999" "#888888"))
     (comment if
              (eq alternative
                  (quote 2))
              "#006600"
              (if
                  (eq alternative
                      (quote 1))
                  "#666666" "#5f8787"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-enforce-face ((t (:background "#161616")))))

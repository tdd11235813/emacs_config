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
  )

(init-theme-dark 0)
;;
;;

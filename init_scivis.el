;;; package --- sci-vis setting
;;; Commentary:
;;;
;;; Code:


(setq max-lisp-eval-depth 1000)
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq package-enable-at-startup nil)
(package-initialize)
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

(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package gnuplot-mode
  :ensure t
  :config
  (defvar gnuplot-program "/usr/bin/gnuplot")
  (add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
  )

;; after download just run make in lisp/ESS/ to byte-compile it
(use-package ess-site
  :load-path "lisp/ESS/lisp/"
  :ensure f
  :config
  (setq ess-history-file nil)
  )

(use-package init_theme
  :load-path "lisp/init/"
  :ensure f
  )

(init-theme-dark 1)
;;;
;;;

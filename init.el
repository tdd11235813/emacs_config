;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Using no variable. Combined with heaven-and-hell.
;;;  Using setq instead of customize-set-variables works, no issues here.
;;; Code:

(package-initialize)

(setq custom-safe-themes t)

;;(load-theme 'test-space t) ;; does not help

;; using heaven-and-hell to switch between themes
(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  ;;(setq heaven-and-hell-theme-type 'light) ;;
  (setq heaven-and-hell-themes
        '((light . test-space-light)
          (dark . test-space))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  :hook (after-init . heaven-and-hell-init-hook) ;;
;;  :hook (emacs-startup . heaven-and-hell-init-hook) ;; if above does not work
  :bind (("C-<f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

;; for the custom variable definitions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
;;
;;

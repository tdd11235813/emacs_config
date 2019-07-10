;;; INIT -- STYLE FILE
;;; Commentary:
;;;  It only works after the spacemacs-theme update where dyn-let has been removed.
;;; Code:

(package-initialize)
(setq custom-safe-themes t)
;; both lines need to be run
(load-theme 'test-space t)
(add-hook 'after-init-hook (lambda() (load-theme 'test-space t)))

;; for the custom variable definitions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
;;
;;

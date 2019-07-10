;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Themes now use a variable.
;;;  It fails after the spacemacs-theme update where dyn-let has been removed.
;;;  Error: Wrong type argument: stringp, my-var
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

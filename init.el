;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Inspired by http://aaronbedra.com/emacs.d/ and several others
;;; Code:

;; to debug on error, use toggle-debug-on-error

;;(add-to-list 'load-path "~/.emacs.d/tmp/benchmark-init-el/")
;;(require 'benchmark-init-loaddefs)
;;(benchmark-init/activate)

;; startup time measuring on terminal
;; $ command time -p emacs -l $HOME/.emacs.d/init.el -Q -e kill-emacs
;; NOTE: Normally, you do not use -l for loading init files!!!

;; byte compile init.el script (not working)
;; emacs -Q --batch -l ~/.emacs.d/init.el -f batch-byte-compile ~/.emacs.d/init.el
;; NOTE: Normally, you do not use -l for loading init files!!!

;; NOTE: describe-char+describe-face to get face details
;; NOTE: emacs ... -Q does not invoke after-init-hook
;; - https://emacs.stackexchange.com/questions/51438/why-after-init-hook-is-not-invoked-workaround-is-emacs-startup-hook

;; NOTE: (setq load-prefer-newer t) ;; use when you want to load modified .el files

(setq max-lisp-eval-depth 1000)
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
;; old init-packages
(when (< emacs-major-version 24)
  (error "Only works on emacs 24 or newer."))
(when (< emacs-major-version 25)
  (warn "Only tested on emacs 25 and newer."))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'quelpa-use-package)
          (package-refresh-contents)
          (package-install 'quelpa-use-package)
          (package-install 'delight)
          )
        ;; (require 'use-package)
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;; if there is use-package, but no quelpa-use-package
(unless (package-installed-p 'quelpa-use-package)
  (when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (package-refresh-contents)
    (package-install 'quelpa-use-package)
    ))
;; --- end of pull request ---

;; do not check for quelpa updates
(setq quelpa-update-melpa-p nil)

;; https://github.com/nilcons/emacs-use-package-fast
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initialization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)

;; (eval-when-compile
(require 'use-package)
(require 'quelpa-use-package)

;; -- use-package settings --

(setq use-package-verbose nil)
;;(setq use-package-verbose t)

(setq use-package-always-ensure nil)
;;(setq use-package-always-ensure t)
;; ... as quelpa is used, put advice to automatically disable ELPA lookup
;; (or add :ensure f for the package that has to be loaded from quelpa)
(quelpa-use-package-activate-advice)

;; -- own packages --

(use-package init_base
  :ensure f
  :load-path "lisp/init"
  )

(use-package init_cpp
  :ensure f
  :load-path "lisp/init"
  )

(use-package init_scivis
  :ensure f
  :load-path "lisp/init"
  )

(use-package heaven-and-hell
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
  (add-to-list 'load-path "~/.emacs.d/lisp/themes")
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . scicpp-light)
          (dark . scicpp-dark))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c M-k" . heaven-and-hell-load-default-theme)
         ("C-c M-l" . heaven-and-hell-toggle-theme))
  )

;; -- custom file
;; for the custom variable definitions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror) ;; noerror when file does not exist
;;
;;

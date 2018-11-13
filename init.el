;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Inspired by http://aaronbedra.com/emacs.d/ and several others
;;; Code:

;; to debug on error, use toggle-debug-on-error

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

;; swiper
;; M-s
;; M-q swiper at point
;; counsel
;; C-c C-g counsel-git-grep
;; neotree
;; C-c t
;; undo-tree
;; C-x u

;; multiple-cursors
;; C-c <right> next-like-this
;; C-c <left> previous-like-this
;; C-x c all-like-this
;; C-c e edit-lines

;; ivy/avy
;; C-l goto line

;; theme
;; C-c M-l lightness

;;(add-to-list 'load-path "~/.emacs.d/tmp/benchmark-init-el/")
;;(require 'benchmark-init-loaddefs)
;;(benchmark-init/activate)

;; startup time measureing on terminal
;; $ command time -p emacs -l $HOME/.emacs.d/init.el -Q -e kill-emacs

;; byte compile init.el script (not working)
;; emacs -Q --batch -l ~/.emacs.d/init.el -f batch-byte-compile ~/.emacs.d/init.el

(setq max-lisp-eval-depth 1000)
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; initially a package-initialize is required to set archives
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (package-initialize)
;;   (package-refresh-contents)
;;   )

;; https://github.com/nilcons/emacs-use-package-fast
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initialization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
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
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package)
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
;; --- end of pull request ---

(eval-when-compile
  (require 'use-package))

;; -- use-package settings --

(setq use-package-verbose nil)
;;(setq use-package-always-ensure nil)
;;(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; -- own packages --

(use-package init_base
  :load-path "lisp/init"
  :ensure f
  )

(use-package init_cpp
 :ensure f
  :load-path "lisp/init"
  )

(use-package init_scivis
 :ensure f
  :load-path "lisp/init"
  )

(use-package init_theme
  :ensure f
  :load-path "lisp/init"
  ;; :bind
  ;; (
  ;;  ("C-c M-d" . init-theme-dark)
  ;;  ("C-c M-l" . init-theme-light)
  ;;  )
  ;; :config
  ;; (init-theme-dark 0)
  :config
  ;;(init-theme-dark 0)
  (init-theme-dark 1)

  )


;; for the custom variable definitions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;
;;

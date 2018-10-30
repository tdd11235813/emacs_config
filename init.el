;;; INIT -- STYLE FILE
;;; Commentary:
;;;  Inspired by http://aaronbedra.com/emacs.d/
;;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
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

;;(add-to-list 'load-path "~/.emacs.d/tmp/benchmark-init-el/")
;;(require 'benchmark-init-loaddefs)
;;(benchmark-init/activate)

;; startup time measureing on terminal
;; $ command time -p emacs -l $HOME/.emacs.d/init.el -Q -e kill-emacs

;; byte compile init.el script (not working)
;; emacs -Q --batch -l ~/.emacs.d/init.el -f batch-byte-compile ~/.emacs.d/init.el

(setq max-lisp-eval-depth 1000)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; https://github.com/nilcons/emacs-use-package-fast
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
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

(setq use-package-verbose nil)
(setq use-package-always-ensure nil)
;;(setq use-package-verbose t)
;;(setq use-package-always-ensure t)

(use-package init_base
  :load-path "lisp/init"
  :ensure f
  )

(use-package init_cpp
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
  )
;;(init-theme-dark 0)

(use-package gnuplot-mode
  :mode ("\\.gnu\\'" . gnuplot-mode) ;; mode implies defer
  :config
  (defvar gnuplot-program "/usr/bin/gnuplot")
;;  (add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
  )

(use-package flyspell
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-local-dictionary "english")
  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "deutsch8") "english" "deutsch8")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)
      ))

  :bind
  (
   ("<f9>" . fd-switch-dictionary)
   )
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(use-package tex-site
  :ensure auctex
  :init
  ;; https://www.emacswiki.org/emacs/AUCTeX
  (setq TeX-PDF-mode t)
  ;;   (setq TeX-parse-self t) ;; parses packages
  (setq-default TeX-master nil) ;; asks for master file after opening

  (setq TeX-source-correlate-method 'synctex)
  ;; ##### Enable synctex generation. Even though the command shows
  ;; ##### as "latex" pdflatex is actually called
  (custom-set-variables '(LaTeX-command "latex -synctex=1") )

  (defun Okular-make-url () (concat
                             "file://"
                             (expand-file-name (funcall file (TeX-output-extension) t)
                                               (file-name-directory (TeX-master-file)))
                             "#src:"
                             (TeX-current-line)
                             (expand-file-name (TeX-master-directory))
                             "./"
                             (TeX-current-file-name-master-relative)))

  ;; ## Use these lines if you want a confirmation of the
  ;; ## command line to run...
  ;; (setq TeX-view-program-selection '((output-pdf "Okular")))
  ;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
  ;; ## And theses if you don't want any confirmation.
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("View" "okular --unique %u" TeX-run-discard-or-function nil t :help "View file")))

  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager) "dvi2tty")
          ((output-dvi style-pstricks) "dvips and gv")
          (output-pdf "Okular")
          (output-dvi "xdvi")
          (output-html "Palemoon")))
  (setq TeX-view-program-list
        '(("Palemoon" "palemoon %o")))
  :hook
  (LaTeX-mode . turn-off-auto-fill)
  (LaTeX-mode . turn-on-visual-line-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . (lambda () (TeX-fold-mode t)))
  (LaTeX-mode . (lambda () (abbrev-mode +1)))
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  ;; ##### Use Okular to open your document at the good
  ;; ##### point. It can detect the master file.
  (LaTeX-mode . (lambda ()(add-to-list 'TeX-expand-list
                                       '("%u" Okular-make-url))))
;; ##### Use Okular to open your document at the good
  ;; ##### point. It can detect the master file.
;;  (add-hook 'LaTeX-mode-hook '(lambda ()
;;                                (add-to-list 'TeX-expand-list
 ;;                                            '("%u" Okular-make-url))))

  ;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
  ;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;;(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))
  )

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t)
  )

(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))
  )

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  ;; enable if needed
  ;;   (add-hook 'prog-mode-hook 'flycheck-mode)
  )

;; after download/update just run make clean && make in lisp/ESS/ to byte-compile it
(use-package ess-site
  :load-path "lisp/ESS/lisp"
  :ensure f
  :defer t
  :config
  (setq ess-history-file nil)
  (ess-toggle-underscore nil)
  )

;; polymode
;; outdated: requires install.packages('rmarkdown') in R and pandoc
;; updated: https://www.reddit.com/r/spacemacs/comments/9ciefe/polymode_for_rmd/
;; requires specific packages
;; FIXME: poly-* might does not work (reinstall?)
(use-package polymode
  :defer t
  :mode
  (
   ("\\.Rmd" . poly-markdown+r-mode)
   ("\\.Snw\\'" . poly-noweb+r-mode)
   ("\\.Rnw\\'" . poly-noweb+r-mode)
   ("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   )
  :init
  (setq markdown-command "multimarkdown")
  (use-package markdown-mode)
  ;;(autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
  (use-package poly-R)
  (use-package poly-noweb )
  (use-package poly-markdown)
  :config
  ;;; MARKDOWN
  ;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  ;;; R modes
  ;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

  (setq markdown-toggle-math t)

  ;; from https://gist.github.com/benmarwick/ee0f400b14af87a57e4a
  ;; compile rmarkdown to HTML or PDF with M-n s
  ;; use YAML in Rmd doc to specify the usual options
  ;; which can be seen at http://rmarkdown.rstudio.com/
  ;; thanks http://roughtheory.com/posts/ess-rmarkdown.html
  (defun ess-rmarkdown ()
    "Compile R markdown (.Rmd). Should work for any output type."
    (interactive)
                                        ; Check if attached R-session
    (condition-case nil
        (ess-get-process)
      (error
       (ess-switch-process)))
    (let* ((rmd-buf (current-buffer)))
      (save-excursion
        (let* ((sprocess (ess-get-process ess-current-process-name))
               (sbuffer (process-buffer sprocess))
               (buf-coding (symbol-name buffer-file-coding-system))
               (R-cmd
                (format "library(rmarkdown); rmarkdown::render(\"%s\")"
                        buffer-file-name)))
          (message "Running rmarkdown on %s" buffer-file-name)
          (ess-execute R-cmd 'buffer nil nil)
          (switch-to-buffer rmd-buf)
          (ess-show-buffer (buffer-name sbuffer) nil)))))

  (define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)
  )

(init-theme-dark 1)


;; for the custom variable definitions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;
;;

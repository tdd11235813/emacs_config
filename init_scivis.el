;;; Package --- sci-vis setting
;;; Commentary:
;;;
;;; Code:
;;; flyspell
;; M-$ show options on a highlighted word (incl. add to dictionary)

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
          (package-install 'use-package))
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
(require 'bind-key)
(require 'diminish)

(setq use-package-verbose nil)


(use-package gnuplot-mode
  :ensure t
  :config
  (defvar gnuplot-program "/usr/bin/gnuplot")
  (add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
  )

;; after download just run make in lisp/ESS/ to byte-compile it
(use-package ess-site
  :load-path "lisp/ESS/lisp"
  :ensure f
  :config
  (setq ess-history-file nil)
  (ess-toggle-underscore nil)
  )

(use-package flyspell
  :ensure t
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-local-dictionary "english")
  :config
  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "deutsch8") "english" "deutsch8")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)
      ))

  (global-set-key (kbd "<f9>")   'fd-switch-dictionary)
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

  ;; ##### Use Okular to open your document at the good
  ;; ##### point. It can detect the master file.
  (add-hook 'LaTeX-mode-hook '(lambda ()
                                (add-to-list 'TeX-expand-list
                                             '("%u" Okular-make-url))))

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
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))
  )

(use-package reftex
  :ensure t
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t)
  )

(use-package bibtex
  :ensure t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(use-package init_base
  :load-path "lisp/init"
  :ensure f
  )

(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )


(use-package init_theme
  :load-path "lisp/init"
  :ensure f
  )

;; requires install.packages('rmarkdown') in R and pandoc
(use-package polymode
  :ensure t
  :config
  (use-package poly-R)
  (use-package poly-markdown)
  ;;; MARKDOWN
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  ;;; R modes
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (markdown-toggle-math t)
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

;;;
;;;

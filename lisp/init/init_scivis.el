;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:


(use-package gnuplot-mode
  :mode ("\\.gnu\\'" . gnuplot-mode) ;; mode implies defer
  :config
  (defvar gnuplot-program "/usr/bin/gnuplot")
;;  (add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
  )

(use-package flyspell
  :init
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-use-meta-tab nil)
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
  :hook
  (text-mode-hook . (lambda ()(flyspell-mode 1)))
  ;; prevent flyspell from finding misspellings in code
  (prog-mode-hook . (lambda () (flyspell-prog-mode)))
  :config
  ;; Set programms http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html#orge18a0ef
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-list-command "--list")

  ;; Refresh flyspell after directory change
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)

  ;; Popup
  (defun flyspell-emacs-popup-textual (event poss word)
    "A textual flyspell popup menu."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (car (cdr (cdr poss))) 'string<)
                       (car (cdr (cdr poss)))))
           (cor-menu (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
           (affix (car (cdr (cdr (cdr poss)))))
           show-affix-info
           (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                       (list
                                        (list (concat "Save affix: " (car affix))
                                              'save)
                                        '("Accept (session)" session)
                                        '("Accept (buffer)" buffer))
                                     '(("Save word" save)
                                       ("Accept (session)" session)
                                       ("Accept (buffer)" buffer)))))
                         (if (consp cor-menu)
                             (append cor-menu (cons "" save))
                           save)))
           (menu (mapcar
                  (lambda (arg) (if (consp arg) (car arg) arg))
                  base-menu)))
      (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))


  (defun flyspell-emacs-popup-choose (org-fun event poss word)
    (if (window-system)
        (funcall org-fun event poss word)
      (flyspell-emacs-popup-textual event poss word)))

  (eval-after-load "flyspell"
    '(progn
       (advice-add 'flyspell-emacs-popup :around #'flyspell-emacs-popup-choose)))
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

(provide 'init_scivis)

;;; init_scivis ends here

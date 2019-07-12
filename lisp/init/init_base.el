;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:


(setq backup-directory-alist `(("." . "~/.emacs.d/saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)



;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English.



;; https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :defer t)
(use-package delight)
(use-package anzu
  :defer t
  :bind
  (
   ("M-%" . anzu-query-replace)
   )
  )
(use-package smex)

(use-package ivy
  :defer 0.1
  ;; :bind
  ;; (
  ;;  ("C-c C-r" . ivy-resume)
  ;;  )
  :delight ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-r" . ivy-previous-line-or-history)
              ("M-r" . ivy-reverse-i-search))
  :config
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil
        ivy-height 15
        ivy-count-format "[%d/%d] "
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)) ;; allow input not in order
        )
  ;;  (use-package ivy-hydra) ;; further key-bindings ?
  (use-package avy ;; move cursor to visible by searching
    :bind
    (
     ("C-v" . avy-goto-line)
     )
    )
  (ivy-mode t)
  )

;; or iedit?
(use-package multiple-cursors
  :defer t
  :bind (("C-c <right>" . mc/mark-next-like-this)
         ("C-c <left>" . mc/mark-previous-like-this)
         ("C-x c" . mc/mark-all-like-this)
         ("C-c e" . mc/edit-lines))
  )

(use-package counsel
  :defer t
  :pin melpa
  :bind
  (
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
;; ("M-y" . counsel-yank-pop) ;; I do not like it
   ("C-c C-g" . counsel-git-grep)
   ;; Use C-j for immediate termination with the current value, and RET
   ;; for continuing completion for that directory. This is the ido
   ;; behaviour.
   :map ivy-minibuffer-map
   (("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done))
   )
  :config
  (setq counsel-find-file-at-point t)
  )

(use-package swiper
  :defer t
  :bind
  (
   ("M-s" . swiper)
   ("M-q" . (lambda () (interactive)
              (swiper (word-at-point))))
   )
  )

(use-package recentf
  :defer t
  :init
  (recentf-mode 1)
  :bind
;;  ("C-c f f" . recentf-open-files)
  ("C-c f f" . counsel-recentf)
  :config
  (setq recentf-max-menu-items 50
        recentf-auto-cleanup 'never
        recentf-keep '(file-remote-p file-readable-p))
  )

(use-package undo-tree
  :defer 1
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode +1)
  )

(use-package company
  :defer t
  :bind
  (("M-f" . company-files))
  ("<backtab>" . company-complete)
  :config
  (setq
   ;;company-backends '((company-files))
        company-idle-delay 1
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 10
        company-require-match nil
        company-async-timeout 6
        company-minimum-prefix-length 1
        company-global-modes '(not term-mode)
        )

  (global-company-mode t)
  (setq company-dabbrev-downcase nil) ;; for case-sensitive autocompletion

  (use-package company-quickhelp
    :config
    (setq company-quickhelp-mode 1)
    (setq company-quickhelp-max-lines 10)
    )
  )

(use-package neotree
  :defer t
  :bind
  ("C-c t" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'arrow)
  )

(use-package org
  :bind
  (
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c v" . org-capture)
   ("C-c q" . org-agenda-list)
   )
  :config
  ;; Org-agenda
  (use-package org-agenda
    :ensure f
    :config
    (custom-set-variables
     '(org-directory "~/.emacs.d/user")
     '(org-agenda-files (list org-directory)))
    (setq org-default-notes-file "~/.emacs.d/user/todo.org")
    (setq org-agenda-show-log t
          ;; org-agenda-todo-ignore-scheduled t
          ;; org-agenda-todo-ignore-deadlines t
          )
  )
  (setq org-archive-location "~/.emacs.d/user/archive/%s_archive::")
  (defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")

  (setq org-log-done t)
  ;; From https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@/!)" "MEETING")))
  ;; Custom colors for the keywords
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("INACTIVE" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("MEETING" :foreground "forest green" :weight bold)))
  ;; Auto-update tags whenever the state is changed
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("INACTIVE" ("WAITING") ("INACTIVE" . t))
          (done ("WAITING") ("INACTIVE"))
          ("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
          ("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
          ("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))

  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
;;  (setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")

  ;; Org-habit
  (use-package org-habit
    :ensure f
    :config
    (add-to-list 'org-modules "org-habit")
    (setq org-modules '(org-habit))
    (setq org-habit-preceding-days 7
          org-habit-following-days 1
          org-habit-graph-column 80
          org-habit-show-habits-only-for-today t
          org-habit-show-all-today t)
    )
  ;; hides blank lines between headings
  (setq org-cycle-separator-lines 0)
  ;;; This is for interleaving and adding of notes in PDFs (notes are stored in single org file)
  ;; https://codearsonist.com/reading-for-programmers
  ;; Add #+INTERLEAVE_PDF: pdf/ to your index.org
  ;; Open it and run M-x interleave
  ;; (https://github.com/rudolfochrist/interleave)
  ;; (pdf-tools installed, see https://github.com/politza/pdf-tools)
  ;; Open PDF with interleave and M-x org-noter + {i,M-i} for adding notes
  ;; Open PDF file and use org-ref-pdf-to-bibtex
  ;; TODO: org-capture for TODO linking
  ;; TODO: check how to automatically download pdfs from urls
  (setq papers-dir (expand-file-name "~/.emacs.d/user/papers")
        papers-pdfs (concat papers-dir "pdf/")
        papers-notes (concat papers-dir "index.org")
        papers-refs (concat papers-dir "index.bib"))

  (use-package interleave)

  (use-package nov
    :mode ("\\.epub\\'" . nov-mode))

  (use-package pdf-tools
    :magic ("%PDF" . pdf-view-mode)
    :config
    (pdf-tools-install))

  (use-package org-ref
    :init
    ;; (setq reftex-default-bibliography (list papers-refs))
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-bibliography-notes papers-notes
          org-ref-default-bibliography (list papers-refs)
          org-ref-pdf-directory papers-pdfs))

  (use-package org-noter
    :after org-mode
    :hook ((pdf-view-mode . org-noter-mode)
           (nov-mode . org-noter-mode))
    :config
    (setq org-noter-notes-search-path (list papers-dir))
    (setq org-noter-default-notes-file-names (list "~/.emacs.d/user/papers/index.org")) ;; does not work with papers-refs, last '/' missed
    )

  (use-package org-cliplink
    :bind
    ("C-x j" . org-cliplink)
    )
  (use-package org-brain)
  (use-package org-clock-today)
  (use-package org-clock-convenience
    :bind (:map org-agenda-mode-map
                ("M-ü" . org-clock-convenience-timestamp-up)
                ("M-ö" . org-clock-convenience-timestamp-down)
                ("ö" . org-clock-convenience-fill-gap)
                ("ä" . org-clock-convenience-fill-gap-both))
    )
  ;; (use-package org-clock)
  ;; (add-to-list
  ;;  'org-clock-clocktable-language-setup
  ;; '("en" "File" "L" "Timestamp" "Task" "Time" "ALL" "Total time" "File time" "Time Sheet at"))
  (setq org-capture-templates
        '(
          ("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
          ("e" "Empty" entry (file org-default-notes-file)
           "* %?\n%u")
          ("m" "Meeting" entry (file org-default-notes-file)
           "* MEETING with %? :MEETING:\n" :clock-in t :clock-resume t)
          ("d" "Diary" entry (file+datetree "~/.emacs.d/user/diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("D" "Daily Log" entry (file "~/.emacs.d/user/daily-log.org")
           "* %u %?\n#+BEGIN: gjs-daily-clocktable :maxlevel 4 :date \"%u\" :link t :compact t \n#+END:\n\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)
          ("i" "Idea" entry (file org-default-notes-file)
           "* %? :IDEA: \n%u" :clock-in t :clock-resume t)
          ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
           "** NEXT %? \nDEADLINE: %t")

          ;; ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
          ;;  "* TODO %?\n  %i\n  %a")
          ;; ("j" "Journal" entry (file+datetree "~/org/journal.org")
          ;;  "* %?\nEntered on %U\n  %i\n  %a")
          ("s" "Code Snippet" entry
           (file (concat org-directory "snippets.org"))
           ;; Prompt for tag and language
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ;; ("m" "Media" entry
          ;;  (file+datetree (concat org-directory "media.org"))
          ;;            "* %?\nURL: \nEntered on %U\n")
          ("r" "Meeting Schedule" entry
           (file+headline file org-default-notes-file "HEADING")
           "* TODO Meeting - %?
%i
Room: %^{Place}
Subject: %^{Subject}
Attendees: %^{Attendees}

%a ")
          ("b" "Bookmark" entry (file+headline "~/.emacs.d/user/bookmarks.org" "Refile")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
          )
        )

  ;; Clocktable (C-c C-x C-r) defaults
  ;; Use fixed month instead of (current-month) because I want to keep a table for each month
  (setq org-clock-clocktable-default-properties
        `(:block ,(format-time-string "%Y-%m") :scope file-with-archives))

  (setq org-duration-format (quote h:mm))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)))

  ;; Clocktable (reporting: r) in the agenda
  (setq org-clocktable-defaults
        '(:maxlevel 3 :lang "en" :scope file-with-archives
                    :wstart 1 :mstart 1 :tstart nil :tend nil :step week :stepskip0 nil :fileskip0 nil
                    :tags nil :emphasize nil :link t :narrow 70! :indent t :formula nil :timestamp nil
                    :level nil :tcolumns nil :formatter nil))

  (use-package ivy-bibtex
    :after ivy
    :config
    (setq bibtex-completion-bibliography papers-refs
          bibtex-completion-library-path papers-pdfs
          bibtex-completion-notes-path papers-notes))

  ;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfe5d909
  (defun my/org-insert-heading-for-next-day ()
    "Insert a same-level heading for the following day."
    (interactive)
    (let ((new-date
           (seconds-to-time
            (+ 86400.0
               (float-time
                (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
      (org-insert-heading-after-current)
      (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

  )

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)
  :init
  ;; do not use git-gutter+tramp:
  ;; https://github.com/syohex/emacs-git-gutter/issues/151
  ;; git-gutter+ gives errors, so we will remove it
;;   (use-package git-gutter+
;; ;;    :delight git-gutter+-mode
;;     :config
;;     (global-git-gutter+-mode)
;;     )
  (use-package git-timemachine)
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;; from https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(use-package dired                      ; Edit directories
  :ensure nil ; see https://emacs.stackexchange.com/questions/26810/why-doesnt-use-package-dired-work-for-me
  :defer t
  :config
  (setq
   dired-auto-revert-buffer t           ; Revert on re-visiting
   ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h' uses
   ;; human-readable sizes, and `-F' appends file-type classifiers to file names
   ;; (for better highlighting)
   dired-listing-switches "-alhF"
   dired-ls-F-marks-symlinks t          ; -F marks links with @
   ;; Inhibit prompts for simple recursive operations
   dired-recursive-copies 'always
   ;; Auto-copy to other Dired split window
   dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq
     dired-listing-switches
     (concat dired-listing-switches " --group-directories-first -v")))

  ;; from https://github.com/vdemeester/emacs-config
  (defun dired-get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-schL" files) ;; -L to dereference (git-annex folder)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*\\(insgesamt\\|total\\)$")
           (match-string 1))))))
  (define-key dired-mode-map (kbd "Z") 'dired-get-size)
  (define-key dired-mode-map "F" 'find-name-dired)
  )


(use-package dired-x                    ; Additional tools for Dired
  :load-path "lisp"
  ;;:quelpa (dired-x :fetcher url :url "https://github.com/emacs-mirror/emacs/raw/master/lisp/dired-x.el") ;; does not download anymore (emacs url retreiver fails), error: missing package desc as dired-x.el is empty
  :defer t
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump))
  :hook
  (dired-mode . dired-omit-mode)
;;  (add-hook 'dired-mode-hook #'dired-omit-mode)
;;  :after dired
  :config
  (setq dired-omit-verbose nil)        ; Shut up, dired

  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar"))

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode "dired"))
                '((name . dired-omit-mode-diminish)))
  ;; from https://github.com/vdemeester/emacs-config
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular")
          ("\\.\\(?:djvu\\|eps\\)\\'" "okular")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "gwenview")
          ("\\.\\(?:xcf\\)\\'" "gimp")
          ("\\.tex\\'" "pdflatex" "latex")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
           "vlc")
          ("\\.\\(?:mp3\\|flac\\)\\'" "vlc")
          ("\\.html?\\'" "palemoon")))
  (put 'dired-find-alternate-file 'disabled nil)
  )

(use-package dired+
  :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
  :defer t
  :init
  (setq diredp-hide-details-initially-flag nil)
  )

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1)
  )


(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-backup-directory-alist backup-directory-alist)
  ;; .ssh/config must be set to use controlpersist options:
  ;; ControlMaster auto
  ;; ControlPath /tmp/%r@%h:%p
  ;; ControlPersist yes
  ;; ^^ required, otherwise 'Sending Password' may hang
  ;; Hint: Do not use tramp it with git-gutter (bad performance)
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

  ;; Disable git backend to speed up sshfs file load among other things
  ;;(setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))
  (setq vc-handled-backends (quote (Git)))
  ;;(setq vc-handled-backends nil)
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;;  (setq tramp-verbose 10)
  (setq tramp-completion-reread-directory-timeout nil)
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  ;; don't generate backups for remote files opened as root (security hazzard)
  ;;(setq disable-tramp-backups nil) ;; allow all tramp files to be backuped
  ;;(setq disable-tramp-backups '("su" "sudo")) ;; only 'su' and 'sudo'
  ;;(setq disable-tramp-backups '("ssh" "sftp")) ;; only 'ssh' and 'sftp'
  (defvar disable-tramp-backups '(all))

  (eval-after-load "tramp"
    '(progn
       ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
       (setq backup-enable-predicate
             (lambda (name)
               (and (normal-backup-enable-predicate name)
                    ;; Disable all tramp backups
                    (and disable-tramp-backups
                         (member 'all disable-tramp-backups)
                         (not (file-remote-p name 'method)))
                    (not ;; disable backup for tramp with the listed methods
                     (let ((method (file-remote-p name 'method)))
                       (when (stringp method)
                         (member method disable-tramp-backups)))))))

       (defun tramp-set-auto-save--check (original)
         (if (funcall backup-enable-predicate (buffer-file-name))
             (funcall original)
           (auto-save-mode -1)))

       (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)
       ))

  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

;; Package: ws-butler
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (cuda-mode . ws-butler-mode)
  )

;; Package: yasnippet
;; check https://github.com/AndreaCrotti/yasnippet-snippets.git
;; https://joaotavora.github.io/yasnippet/snippet-development.html
(use-package yasnippet
  :defer 1
  :delight yas-minor-mode
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package dumb-diff
  :bind (("C-c 1" . dumb-diff-set-region-as-buffer1)
         ("C-c 2" . dumb-diff-set-region-as-buffer2)
         ("C-c q" . dumb-diff-quit))
  :ensure t)

;; buffer cleanup
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))


(defun swap-window-positions ()         ; Stephen Gildea
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))

;; from https://github.com/howardabrams/dot-files/blob/master/emacs.org
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
        (fill-paragraph nil)))

(use-package expand-region
  :ensure t
  :config
  (defun ha/expand-region (lines)
    "Prefix-oriented wrapper around Magnar's `er/expand-region'.

Call with LINES equal to 1 (given no prefix), it expands the
region as normal.  When LINES given a positive number, selects
the current line and number of lines specified.  When LINES is a
negative number, selects the current line and the previous lines
specified.  Select the current line if the LINES prefix is zero."
    (interactive "p")
    (cond ((= lines 1)   (er/expand-region 1))
          ((< lines 0)   (ha/expand-previous-line-as-region lines))
          (t             (ha/expand-next-line-as-region (1+ lines)))))

  (defun ha/expand-next-line-as-region (lines)
    (message "lines = %d" lines)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line lines))

  (defun ha/expand-previous-line-as-region (lines)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line (1+ lines)))
  :bind ("C-f" . ha/expand-region))

(use-package saveplace
  :defer 0
  :config
  (save-place-mode)
  (setq save-place-forget-unreadable-files t
        save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"
        save-place-file "~/.emacs.d/places")
  (save-place-mode t)
  )

(defun align-comma (start end c)
  "Repeat alignment with a character padded with spaces for
comma-separated columns."
  (interactive "r\nsAlign character: ")
  (align-regexp start end
                (concat c "\\(\\s-*\\)") 1 1 t))


(use-package hideshow
  ;;:quelpa (hideshow :fetcher url :url "https://github.com/jwiegley/emacs-release/raw/master/lisp/progmodes/hideshow.el") ;; does not download anymore, see comment above (dired-x)
  :load-path "lisp"
  :ensure f
  :init
  (defun ha/hs-show-all ()
    (interactive)
    (hs-minor-mode 1)
    (hs-show-all))

  (defun ha/hs-hide-all ()
    (interactive)
    (hs-minor-mode 1)
    (hs-hide-all))

  (defun ha/hs-toggle-hiding ()
    (interactive)
    (hs-minor-mode 1)
    (hs-toggle-hiding))
  :bind
;;  ("C-c s t" . hs-minor-mode)
  ("C-c s d" . ha/hs-hide-all)
  ("C-c s a" . ha/hs-show-all)
  ("C-c s s" . ha/hs-toggle-hiding))


(use-package duplicate-thing
  :bind
  (("C-d" . duplicate-thing))
  )

;; tried ibuffer, but buffer-list of bs is just more streamlined.
;; ibuffer also forced tramp to reload even if next buffer is not in tramp mode.
(use-package bs
  :init
  (use-package misc-cmds
    :quelpa (misc-cmds :fetcher github :repo "emacsmirror/misc-cmds")
    )
  :bind
  (("C-x <left>" . previous-buffer-repeat)
   ("C-x <right>" . next-buffer-repeat)
   ("<f6>" . bs-show)
   ("<f5>" . revert-buffer-no-confirm)
   ("<C-next>" . bs-cycle-previous)
   ("<C-prior>" . bs-cycle-next)
   )
  :config
  (add-to-list 'bs-configurations
               '("emacs" nil nil nil
                 (lambda (buf)
                   (with-current-buffer buf
                     (not (memq major-mode
                                '(emacs-lisp-mode))))) nil))
  (add-to-list 'bs-configurations
               '("org" nil nil nil
                 (lambda (buf)
                   (with-current-buffer buf
                     (not (memq major-mode
                                '(org-mode))))) nil))
  (add-to-list 'bs-configurations
               '("C" nil nil nil
                 (lambda (buf)
                   (with-current-buffer buf
                     (not (memq major-mode
                                '(c-mode c++-mode cuda-mode cmake-mode glsl-mode))))) nil))
  )


(use-package drag-stuff
  :delight drag-stuff-mode
  :bind
  (("M-<up>" . drag-stuff-up)
   ("M-<down>" . drag-stuff-down)
   ("M-<home>" . beginning-of-buffer)
   ("M-<end>" . end-of-buffer)
   ("M-<delete>" . kill-word)
   )
  :config
  (drag-stuff-global-mode t)
  )

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package subword
  :hook
  (prog-mode . subword-mode))

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ;; ("M-g x" . dumb-jump-go-prefer-external)
         ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
         )
  :config
  (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  (setq dumb-jump-max-find-time 10)
  (setq my-ag-ignored-patterns '("*.bak" "*.out" "*.csv" "*.pyc" "*.pdf" "*.png" "*.jpg"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "c++" :ext "cu"))
  )

(use-package cd-compile
  :defer t
  :bind (:map prog-mode-map
         ("C-c c" . compile)
         ("C-c x" . my-compile)
         ("C-c y" . kill-compilation))
  :config

  (defun my-compile ()
    "Run compile and resize the compile window"
    (interactive)
    (progn
      (call-interactively 'cd-compile)
      (setq cur (selected-window))
      (setq w (get-buffer-window "*compilation*"))
      (select-window w)
      (setq h (window-height w))
      (shrink-window (- h 10))
      (select-window cur)
      )
    )
  )


(use-package general
  :defer 0
  :bind
  (
   ("C-c d" . kill-whole-line)
   ("C-;" . comment-or-uncomment-region)
   ("C-c C-k" . ff-find-other-file)
   ("C-x <up>" . other-window)
   ("C-x <down>" . previous-multiframe-window)
   ("C-M-<prior>" . scroll-down-line)
   ("C-M-<next>" . scroll-up-line)
   ("M-{" . insert-pair)
   ("M-'" . insert-pair)
   ("M-\"" . insert-pair)
   ("M-g k" . align-regexp)   ;; to align relative to expression
   ("C-x M-t" . cleanup-region)
   ("C-c n" . cleanup-buffer)
   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)
   )
  :config
  ;; fix broken keys
  (define-key global-map "\M-[1~" 'beginning-of-line)
  (define-key global-map [select] 'end-of-line)
  (setq select-enable-clipboard t)
  ;; show empy line markers, file endings
  (setq-default indicate-empty-lines t)
  (when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))
  ;; remove tabs
  (setq-default tab-width 2
                indent-tabs-mode nil)

  ;; one-character answer
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq initial-scratch-message "")
  (when (window-system)
    (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
    (when (fboundp 'horizontal-scroll-bar-mode)
      (horizontal-scroll-bar-mode -1))
    (scroll-bar-mode -1)            ;; Scrollbars are waste screen estate
    (add-to-list 'default-frame-alist '(height . 32))
    (add-to-list 'default-frame-alist '(width . 120))
    )

  (setq echo-keystrokes 0.1
        use-dialog-box nil
        visible-bell t)
  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll

  (put 'downcase-region 'disabled nil)
  (delete-selection-mode t)
  (transient-mark-mode t)
  (customize-set-variable 'show-trailing-whitespace t)
  ;; C-k
  (customize-set-variable 'kill-whole-line t)
  ;; Paste text where the cursor is, not where the mouse is.
  ;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
  (customize-set-variable 'mouse-yank-at-point t)

  ;; Split windows in Emacs 22 compatible way
  ;; (setq split-height-threshold nil)
  ;; (setq split-width-threshold most-positive-fixnum)

  )

(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(provide 'init_base)

;;; init_base ends here

;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:

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
     '(org-directory user-dir)
     '(org-agenda-files (list org-directory)))
    (setq org-default-notes-file (concat user-dir "todo.org"))
    (setq org-agenda-show-log t
          ;; org-agenda-todo-ignore-scheduled t
          ;; org-agenda-todo-ignore-deadlines t
          )
  )
  (setq org-archive-location (concat user-archive-dir "%s_archive::"))
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
  (setq papers-pdfs (concat user-papers-dir "pdf/")
        papers-notes (concat user-papers-dir "index.org")
        papers-refs (concat user-papers-dir "index.bib"))

  (use-package interleave)

  (use-package nov
    :mode ("\\.epub\\'" . nov-mode))

  ;; (use-package pdf-tools
  ;;   :magic ("%PDF" . pdf-view-mode)
  ;;   :config
  ;;   (pdf-tools-install))

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

(provide 'init_org)

;;; init_base ends here

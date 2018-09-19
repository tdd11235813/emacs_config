;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:


(setq backup-directory-alist `(("." . "~/.emacs.d/user/saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(use-package org
  :init
  (use-package deft
    :config
    (setq deft-directory "~/.emacs.d/user/deft")
    (setq deft-use-filename-as-title t)
    (setq deft-extensions "org")
    )
  :config
  (setq org-log-done t
        org-todo-keywords '((sequence "TODO" "STARTED" "CANCELLED" "DONE"))
        org-todo-keyword-faces '(("STARTED" . (:foreground "blue" :weight bold)) ("CANCELLED" . (:foreground "red" :weight bold))))

  ;; Org-agenda
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (setq org-agenda-show-log t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-files (list "~/.emacs.d/user/org/personal.org"
                               "~/.emacs.d/user/org/groupon.org"))
                                        ; Org-habit
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

(use-package sr-speedbar
  :bind
  (("M-ü" . sr-speedbar-toggle)
   ("M-Ü" . sr-speedbar-refresh-toggle))
  :init
  ;; also show dotted directories.
  (setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
  :config
  (setq speedbar-use-images nil)
  ;; Make Sr-speedbar open files in the next window, instead of in the previous window
  (defun select-next-window ()
    (other-window 1))
  (defun my-sr-speedbar-open-hook ()
    (add-hook 'speedbar-before-visiting-file-hook 'select-next-window t)
    (add-hook 'speedbar-before-visiting-tag-hook 'select-next-window t)
    )
  (advice-add 'sr-speedbar-open :after #'my-sr-speedbar-open-hook)
  (speedbar-add-supported-extension ".cu")
  (add-to-list 'speedbar-fetch-etags-parse-list
               '("\\.cu" . speedbar-parse-c-or-c++tag))
  )
(use-package diminish
  :ensure t
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
  :defer t
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
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
  :load-path "lisp"
  :init
  (setq diredp-hide-details-initially-flag nil)
  )

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))




(use-package company
  :bind
  (("M-f" . company-files))
  :config
  (global-company-mode t)
  ;; (use-package fill-column-indicator
  ;;   :config
  ;;   (fci-mode 1)
  ;;   (setq fci-rule-character-color "black"
  ;;         fci-rule-column 80)
  ;;   (defun on-off-fci-before-company(command)
  ;;     (when (string= "show" command)
  ;;       (turn-off-fci-mode))
  ;;     (when (string= "hide" command)
  ;;       (turn-on-fci-mode)))
  ;;   (advice-add 'company-call-frontends :before #'on-off-fci-before-company)
  ;;   ;;(add-hook 'after-change-major-mode-hook 'fci-mode)
  ;;   (add-hook 'prog-mode-hook 'fci-mode)
  ;;   )
  )

;; markdown
(use-package markdown-mode
  :mode "\\.md$"
  :config
  (setq markdown-command "pandoc --smart -f markdown -t html")
  )

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-backup-directory-alist backup-directory-alist)
  ;; .ssh/config must be set to use controlpersist options:
  ;; ControlMaster auto
  ;; ControlPath /tmp/%r@%h:%p
  ;; ControlPersist yes
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Disable git backend to speed up sshfs file load among other things
  ;;(setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))
  (setq vc-handled-backends nil)
  )

(use-package ido
  :config
  (use-package ido-hacks
    :config
    (ido-hacks-mode 1)
    )
  (use-package ido-vertical-mode
    :config
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    )
  (ido-mode t)
  (ido-everywhere 1)
  (ido-vertical-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-use-filename-at-point 'guess
        )
  )

;; Package: ws-butler
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'cuda-mode-hook 'ws-butler-mode)
  )

;; Package: yasnippet
;; check https://github.com/AndreaCrotti/yasnippet-snippets.git
;; https://joaotavora.github.io/yasnippet/snippet-development.html
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  )

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

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

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

(use-package recentf
  :init
  (setq recentf-max-menu-items 25
        recentf-auto-cleanup 'never
        recentf-keep '(file-remote-p file-readable-p))
  (recentf-mode 1)
  (let ((last-ido "~/.emacs.d/ido.last"))
    (when (file-exists-p last-ido)
      (delete-file last-ido)))
  :bind ("C-c f f" . recentf-open-files))

(setq save-place-forget-unreadable-files t
      save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"
      save-place-file "~/.emacs.d/places")
(save-place-mode t)

(defun align-comma (start end c)
  "Repeat alignment with a character padded with spaces for
comma-separated columns."
  (interactive "r\nsAlign character: ")
  (align-regexp start end
                (concat c "\\(\\s-*\\)") 1 1 t))


(use-package hideshow
  :load-path "lisp"
  :ensure t
  :pin manual
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
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . ha/hs-hide-all)
  ("C-c h s" . ha/hs-show-all)
  ("C-c h h" . ha/hs-toggle-hiding))


(use-package misc-cmds
  :load-path "lisp"
  :config
  (use-package bs
    :bind
    (
     ("C-x <left>" . previous-buffer-repeat)
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
    :diminish drag-stuff-mode)
  (drag-stuff-global-mode t)
  (global-set-key (kbd "M-<up>") 'drag-stuff-up)
  (global-set-key (kbd "M-<down>") 'drag-stuff-down)
  (global-set-key (kbd "M-<home>") 'beginning-of-buffer)
  (global-set-key (kbd "M-<end>") 'end-of-buffer)
  (global-set-key "\C-d" 'duplicate-line) ; clone line
  (global-set-key [M-delete] 'kill-word)
  (global-set-key (kbd "C-c d") 'kill-whole-line)
  ;; final customizations
  (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; fix broken keys
  (define-key global-map "\M-[1~" 'beginning-of-line)
  (define-key global-map [select] 'end-of-line)

  (global-set-key (kbd "C-c C-k") 'ff-find-other-file)
  (define-key global-map (kbd "C-x <up>") 'other-window)
  (define-key global-map (kbd "C-x <down>") 'previous-multiframe-window)

  (global-set-key (kbd "C-M-<prior>") 'scroll-down-line)
  (global-set-key (kbd "C-M-<next>") 'scroll-up-line)

  (global-set-key (kbd "M-{") 'insert-pair)
;;  (global-set-key (kbd "M-<") 'insert-pair)
  (global-set-key (kbd "M-'") 'insert-pair)
  (global-set-key (kbd "M-\"") 'insert-pair)

  (setq select-enable-clipboard t)
                                        ; show empy line markers, file endings
  (setq-default indicate-empty-lines t)
  (when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))
                                        ; remove tabs
  (setq-default tab-width 2
                indent-tabs-mode nil)
                                        ; one-character answer
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq initial-scratch-message "")
  (when (window-system)
    (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
    (when (fboundp 'horizontal-scroll-bar-mode)
      (horizontal-scroll-bar-mode -1))
    (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

  (setq echo-keystrokes 0.1
        use-dialog-box nil
        visible-bell t)
  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll

  (put 'downcase-region 'disabled nil)
  (global-set-key (kbd "C-x M-t") 'cleanup-region)
  (global-set-key (kbd "C-c n") 'cleanup-buffer)
  (delete-selection-mode t)
  (transient-mark-mode t)
  )

(setq company-dabbrev-downcase nil) ;; for case-sensitive autocompletion

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g b" . dumb-jump-back)
         ;; ("M-g x" . dumb-jump-go-prefer-external)
         ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
         )
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    :ensure)

(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(provide 'init_base)

;;; init_base ends here

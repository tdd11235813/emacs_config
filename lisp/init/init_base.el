;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:

;; basic function to install packages if needed,
;; but use-package normally will do that job for us
;; (defun init-base-load (pkg_list)
;;   "Load packages listed in PKG_LIST."
;;   (cl-loop for pkg in pkg_list do
;;            (unless (package-installed-p pkg)
;;              (message "Refreshing package database...")
;;              (package-refresh-contents)
;;              (package-install pkg)
;;            )))


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
(use-package yasnippet
  :config
  (yas-global-mode 1)
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


(use-package misc-cmds
  :config
  (use-package bs
    :bind
    (
     ("C-x <left>" . previous-buffer-repeat)
     ("C-x <right>" . next-buffer-repeat)
     ("<f6>" . bs-show)
     ("<f5>" . revert-buffer-no-confirm)
     ("<C-prior>" . bs-cycle-previous)
     ("<C-next>" . bs-cycle-next)
     )
    :config
    ;; (global-set-key [remap bs-cycle-previous] 'bs-cycle-previous-repeat) ; ctrl+LeftArrow
    ;; (global-set-key [remap bs-cycle-next]     'bs-cycle-next-repeat)
    (add-to-list 'bs-configurations
                 '("C" nil nil nil
                   (lambda (buf)
                     (with-current-buffer buf
                       (not (memq major-mode
                                  '(c-mode c++-mode cuda-mode))))) nil))
    )
  (use-package drag-stuff)
  (drag-stuff-global-mode t)
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
  (setq echo-keystrokes 0.1
        use-dialog-box nil
        visible-bell t)
  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time

  (setq backup-directory-alist `(("." . "~/.emacs.d/user/saves"))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (put 'downcase-region 'disabled nil)
  (global-set-key (kbd "C-x M-t") 'cleanup-region)
  (global-set-key (kbd "C-c n") 'cleanup-buffer)
  (delete-selection-mode t)
  (transient-mark-mode t)
  )

(provide 'init_base)

;;; init_base ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(org-agenda-files (list org-directory))
 '(org-directory "~/.emacs.d/user")
 '(package-selected-packages
   (quote
    (ivy-bibtex org-clock-convenience org-clock-today org-brain org-cliplink org-noter org-ref pdf-tools nov interleave yasnippet yaml-mode ws-butler which-key wgrep undo-tree sr-speedbar spacemacs-theme spaceline smex rtags rainbow-delimiters quelpa-use-package ppindent powerthesaurus poly-R neotree multiple-cursors modern-cpp-font-lock misc-cmds magit json-mode ido-vertical-mode ido-hacks highlight-numbers hideshow gnuplot-mode git-timemachine general flycheck expand-region duplicate-thing dumb-jump drag-stuff dired-x dired+ diminish delight deft counsel company column-enforce-mode cmake-ide cmake-font-lock cd-compile avy auto-complete-clang auctex anzu academic-phrases)))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-custom-colors
   (quote
    ((str if
          (eq alternative
              (quote 2))
          "#aa0000" "#ff8866")
     (act1 if
           (eq alternative
               (quote 2))
           "#888888"
           (if
               (eq alternative
                   (quote 1))
               "#000030" "#303030"))
     (act2 if
           (eq alternative
               (quote 2))
           "#999999"
           (if
               (eq alternative
                   (quote 1))
               "#111111" "#404040"))
     (lnum . "#999999")
     (highlight if
                (eq alternative
                    (quote 2))
                "#cccccc" "#444444")
     (green-bg-s if
                 (eq alternative
                     (quote 2))
                 "#cccccc" "#444444")
     (bg1 if
          (eq alternative
              (quote 2))
          "#ffffff"
          (if
              (eq alternative
                  (quote 1))
              "#202020" "#262626"))
     (keyword if
              (eq alternative
                  (quote 2))
              "#2200dd"
              (if
                  (eq alternative
                      (quote 1))
                  "#22ddff" "#99cc55"))
     (const if
            (eq alternative
                (quote 2))
            "#000000" "#ffffff")
     (type if
           (eq alternative
               (quote 2))
           "#000044" "#88ee88")
     (var if
          (eq alternative
              (quote 2))
          "#000000" "#aaffaa")
     (func if
           (eq alternative
               (quote 2))
           "#0000aa" "#ff99ff")
     (base if
           (eq alternative
               (quote 2))
           "#101010"
           (if
               (eq alternative
                   (quote 1))
               "#cccccc" "#b2b2b2"))
     (base-dim if
               (eq alternative
                   (quote 2))
               "#121212"
               (if
                   (eq alternative
                       (quote 1))
                   "#999999" "#888888"))
     (comment if
              (eq alternative
                  (quote 2))
              "#006600"
              (if
                  (eq alternative
                      (quote 1))
                  "#666666" "#5f8787")))))
 '(tramp-use-ssh-controlmaster-options nil nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-enforce-face ((t (:background "#161616")))))

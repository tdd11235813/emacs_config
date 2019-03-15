(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(cmake-ide-flags-c "-std=c++11")
 '(cmake-ide-flags-c++ "-std=c++11")
 '(company-clang-arguments (quote ("--gcc-toolchain=/sw/global/compilers/gcc/5.3.0")))
 '(flycheck-c/c++-clang-executable
   "/sw/global/compilers/llvm/3.7/bin/clang++ --gcc-toolchain=/sw/global/compilers/gcc/5.3.0")
 '(gc-cons-threshold 50000000)
 '(gnutls-min-prime-bits 2048)
 '(kill-whole-line t)
 '(mouse-yank-at-point t)
 '(org-agenda-files (list org-directory))
 '(org-directory "~/.emacs.d/user")
 '(package-selected-packages
   (quote
    (ivy-bibtex org-clock-convenience org-clock-today org-brain org-cliplink org-noter org-ref pdf-tools nov interleave ivy-yasnippet general which-key avy powerthesaurus academic-phrases poly-R json-mode duplicate-thing git-timemachine git-gutter neotree undo-tree swiper counsel multiple-cursors ivy smex anzu wgrep cd-compile magit delight dumb-jump polymode auctex gnuplot-mode yaml-mode travis php-mode spaceline rainbow-delimiters highlight-numbers column-enforce-mode spacemacs-theme glsl-mode modern-cpp-font-lock cmake-ide rtags cmake-mode auto-complete-clang flycheck cuda-mode drag-stuff misc-cmds yasnippet ws-butler ido-vertical-mode ido-hacks markdown-mode company sr-speedbar deft f use-package)))
 '(show-trailing-whitespace t)
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
 '(tramp-use-ssh-controlmaster-options nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-enforce-face ((t (:background "#161616")))))

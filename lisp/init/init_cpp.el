;;; package --- org setting
;;; Commentary:
;;;  Packs org based settings for init
;;; Code:

(setq-default c-default-style "linux"
              c-basic-offset 2)
(setq-default tab-width 2
              indent-tabs-mode nil)

(use-package cuda-mode
  ;;    :load-path "lisp/"
  :mode "\\.cu$"
  )

(use-package flycheck
  :diminish flycheck-mode)

(use-package cc-mode)

(use-package company-clang
  :ensure auto-complete-clang
  )

(use-package cmake-mode)
;; git clone --recursive https://github.com/Andersbakken/rtags.git (requires llvm, gcc with c++11 support)
;; probably set make-ide-rdm-executable and cmake-ide-rc-executable
(use-package rtags
    :bind
    (
     :map c-mode-base-map
          ("M-." . rtags-find-symbol-at-point)
          ("M-," . rtags-find-references-at-point)
          ("<backtab>" . company-complete)
          ("C-d" . duplicate-line) ; overwrite cc-cmds. from init_base.el.
          )
    )

(use-package cmake-ide
  :config
  (defun cide-hook ()
    "Enables cmake-ide environment."
    ;; probably set cmake-ide-flags-c++ with flags and include paths (eg. by gcc -v -xc++ /dev/null -fsyntax-only)
    ;; if cmake needs specific flags, then you might customize by cmake-ide-cmake-command or move directly to the build directory to run cmake manually
    ;; with CMAKE_EXPORT_COMPILE_COMMANDS=ON (cmake-ide needs that compile_commands.json)
    ;;
    ;; ...
    ;; (setq cmake-ide-build-dir "build")
    (setq cmake-ide-build-pool-dir "~/.cmake-ide/build")
    (setq cmake-ide-build-pool-use-persistent-naming t)
    (cmake-ide-setup)
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    )
  (defun cide-hook-do ()
    "Enables cmake-ide environment."
    (interactive)
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook 'cide-hook)
    (add-hook 'c-mode-hook 'cide-hook)
    (revert-buffer)
    )
  :bind
  (
   :map c-mode-base-map
        ("M-ä" . cide-hook-do)
        )
  )


(use-package modern-cpp-font-lock
  :config
  ;;    (add-hook 'cuda-mode-hook #'modern-c++-font-lock-mode) ; will conflict (test within [template] function signature: write unsigned const)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  )

(use-package glsl-mode
  :load-path "lisp/glsl-mode/"
  :config
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  )

(defun indent2 ()
  (interactive)
  (setq c-basic-offset 2))
(defun indent4 ()
  (interactive)
  (setq c-basic-offset 4))
(global-set-key (kbd "C-c ö") 'indent2)
(global-set-key (kbd "C-c ä") 'indent4)

;; color-codes
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    "Colorize the compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )

(provide 'init_cpp)
;;; init_cpp.el ends here

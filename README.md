# Emacs config

## Features

- C++11 Support with code completion and refactoring features by rtags and cmake-ide (as opt-in)
- improved, but still incomplete syntax highlighting (aimed for personal code style)
- CUDA, gnuplot, R
- **init.el**: for coding (C, C++, CUDA, GLSL, ...)
- **init_scivis.el**: for scientific visualization and organization (R, gnuplot, LaTeX, org-mode, ..)

## Keys

### Navigation
| Command               | Description                         |
| :---                  | :---                                |
| `C-x <Up/Down>`       | cycle windows                       |
| `C-x <Left/Right>`    | cycle buffers                       |
| `C-<PageDown/PageUp>` | cycle source files only             |
| `F6`                  | bs-show (opened sources files list) |
| `F5`                  | reload buffer                       |
| `C-M-<prior/next>`    | scroll-down/up-line                 |
| `M-<PageDwn>`         | scroll other window                 |
| `C-M-s`               | regexp search                       |
| `C-f`                 | expand-region                       |
| `C-M <up/down>`       | move through level of parentheses   |
| `C-c f j`             | dired-jump                          |
| `C-c f f`             | open recent files                   |
| `C-c f b`             | list bookmarks                      |
| `C-x g`               | magit-status                        |

### Textwork

| Command               | Description                         |
| :---                  | :---                                |
| `C-d`                 | duplicate line                      |
| `C-c d`               | kill line                           |
| `M-f`                 | complete filename (company-files)   |
| `M-<up>`              | drag line up                        |
| `M-<down>`            | drag line down                      |
| `C-;`                 | (un)comment region                  |
| `C-c n`               | cleanup buffer                      |
| `<f9>`                | switch dictionary (en, de) (init_scivis.el) |

### Coding (with cmake-ide and rtags enabled)

| Command               | Description                         |
| :---                  | :---                                |
| `M-ä`                 | enable cmake-ide environment        |
| `M-ü`                 | speedbar (`M-Ü` to refresh)         |
| `<Backtab>`           | code completion (company complete)  |
| `M-.`                 | find symbol/definition/.. at point (rtags) |
| `M.,`                 | find references at point (rtags)    |
| `M-m`                 | start of code line                  |
| `C-c ö`               | switch to 2-space indentation style |
| `C-c ä`               | switch to 4-space indentation style |

### Misc

| Command               | Description                         |
| :---                  | :---                                |
| `M-/`                 | dabbrev-expand          |
| `M-r`                 | line to bottom top      |
| `M-\`                 | delete whitespaces      |
| `M-^`                 | join to previous line   |
| `C-M-x`               | eval defun (lisp)       |
| `C-c M-l`             | toggle theme lightness  |

### Folding

| Command               | Description                         |
| :---                  | :---                                |
| `C-c T h`             | hs-minor-mode                       |
| `C-c h a`             | ha/hs-hide-all                      |
| `C-c h s`             | ha/hs-show-all                      |
| `C-c h h`             | ha/hs-toggle-hiding                 |

...

## Install

- Run recursive git clone for loading submodules glsl-mode and ESS:
```bash
git clone --recursive https://github.com/tdd11235813/emacs_config.git
```
  - Run `make` in lisp/ESS.

- [rtags](https://github.com/Andersbakken/rtags) is required (which requires LLVM/Clang compiler):
```bash
git clone --recursive https://github.com/Andersbakken/rtags.git
```

- For auto-installing further emacs packages, uncomment in init.el and init_scivis.el:
```lisp
(setq use-package-always-ensure t)
```
- For cmake-ide build-pool-dir is used (`cmake-ide-build-pool-dir "~/.cmake-ide/build"`).
For further hints see `init_cpp.el`.

## Emacs alias
C++ IDE, CUDA, ..
```bash
alias em="emacs -nw -l ~/.emacs.d/init.el"
alias ems="em --daemon=workspaceC"
alias emc="emacsclient -nw -s workspaceC"
```
R (using ESS), gnuplot
```bash
alias emsci="emacs -nw -l ~/.emacs.d/init_scivis.el"
alias emssci="emsci --daemon=workspaceS"
alias emcsci="emacsclient -nw -s workspaceS"
```

## Screenshots

CUDA Code (no cmake-ide, no rtags support)
![Screenshot Emacs with CUDA Code](/images/screenshot.jpg)

C++ Code (with cmake-ide and rtags support)
![Screenshot Emacs with C++ Code](/images/screenshot01.jpg)

R Code (run with init_scivis.el as Emacs init file)
![Screenshot Emacs with R Code](/images/screenshot02.jpg)

Emacs Window
![Screenshot Emacs Window](/images/screenshot03.jpg)

## Used Packages

```
ansi-color
bibtex
bookmark
bs
cc-mode
cmake-ide
cmake-mode
column-enforce-mode
company
company-clang
cuda-mode
deft
dired
dired-x
drag-stuff
ess-site
expand-region
fill-column-indicator
flycheck
flyspell
glsl-mode
gnuplot-mode
highlight-numbers
hs-minor-mode
ido
ido-hacks
ido-vertical-mode
init_base
init_theme
linum
markdown-mode
misc-cmds
modern-cpp-font-lock
org
org-habit
poly-R
poly-markdown
polymode
rainbow-delimiters
recentf
reftex
rtags
semantic
spaceline-config
spacemacs-common
sr-speedbar
tex-site
tramp
ws-butler
yasnippet
```

## License

Public Domain (using [CC0](https://creativecommons.org/publicdomain/zero/1.0/)).

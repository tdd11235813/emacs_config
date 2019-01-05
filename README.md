# Emacs config

## Features

- Supports various languages such as C++11, CUDA, CMake, GLSL, gnuplot, R, LaTeX, ...
- dumb-jump for navigating through code
- magit, git-timemachine for git
- neotree for folder navigation
- ESS for R
- polymode, poly-markdown, poly-R for files that contain multiple languages
- org-ref + org-noter + pdf-tools for paper reading (`user/` is a separate, private, versioned folder)
- undo-tree is optional
- counsel only where it helps
- hideshow for code folding
- quelpa-use-package for package installation and configuration
- Tuned startup, but emacsclient/server setup is recommended
- custom theme, that has dark and bright mode (toggle by key `C-c M-l`)
- Note: some of the key mappings contain German umlauts

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
| `C-c C-g`             | counsel-git-grep                    |
| `M-s`                 | swiper                              |
| `M-q`                 | swiper at point                     |
| `C-c t`               | neotree (folder navigation)         |
| `C-x u`               | undo-tree                           |

### Textwork

| Command       | Description                         |
| :---          | :---                                |
| `C-d`         | duplicate line                      |
| `C-c d`       | kill line                           |
| `M-f`         | complete filename (company-files)   |
| `M-a`         | start of sentence                   |
| `M-m`         | start of code line                  |
| `M-r`         | cursor bottom-center-top            |
| `M-<up>`      | drag line up                        |
| `M-<down>`    | drag line down                      |
| `M-\`         | delete whitespaces                  |
| `C-;`         | (un)comment region                  |
| `C-c n`       | cleanup buffer                      |
| `<f9>`        | switch dictionary (en, de)          |
| `<Backtab>`   | code completion (company complete)  |
| `C-c ö`       | switch to 2-space indentation style |
| `C-c ä`       | switch to 4-space indentation style |

#### Multiple Cursors

| Command       | Description                         |
| :---          | :---                                |
| `C-c <right>` | next-like-this                      |
| `C-c <left>`  | previous-like-this                  |
| `C-x c`       | all-like-this                       |
| `C-c e`       | edit-lines                          |

### Coding (jump-to without rtags)

| Command | Description               |
| :---    | :---                      |
| `M-g o` | dumb-jump-go-other-window |
| `M-g j` | dumb-jump-go              |
| `M-g i` | dumb-jump-go-prompt       |

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
academic-phrases
ansi-color
anzu
avy
bibtex
bind-key
bookmark
bs
cc-mode
cd-compile
cmake-font-lock
cmake-mode
column-enforce-mode
company
company-quickhelp
counsel
cuda-mode
deft
delight
dired
dired+
dired-x
display-line-numbers
drag-stuff
dumb-jump
duplicate-thing
ess-site
expand-region
fill-column-indicator
flycheck
flyspell
general
git-gutter+
git-timemachine
glsl-mode
gnuplot-mode
hideshow
highlight-numbers
interleave
ivy
ivy-bibtex
ivy-hydra
ivy-yasnippet
json-mode
linum
magit
markdown-mode
misc-cmds
modern-cpp-font-lock
multiple-cursors
neotree
nov
org
org-habit
org-noter
org-ref
pandoc-mode
pdf-tools
poly-markdown
polymode
poly-noweb
poly-R
powerthesaurus
ppindent
rainbow-delimiters
recentf
reftex
saveplace
semantic/sb
smex
spaceline-config
spacemacs-common
subword
swiper
tex-site
tramp
undo-tree
wgrep
which-key
ws-butler
yaml-mode
yasnippet
zotxt
```

## License

Public Domain (using [CC0](https://creativecommons.org/publicdomain/zero/1.0/)).

# Emacs config

## Features

- Supports various languages such as C++11, CUDA, CMake, GLSL, gnuplot, R, LaTeX, ...
- `dumb-jump` for navigating through code ([use](https://github.com/jacktasia/dumb-jump#configuration) a `.dumbjump` file to exclude folders is recommended)
- `magit`, `git-timemachine` for git
- `neotree` for folder navigation
- `polymode` to support multiple languages in a file
- org-ref + org-noter + pdf-tools for paper reading (`user/` is a separate, private, versioned folder)
- org capture for creating diaries with work times and ToDo/notes lists (support for archiving todo points)
- `hideshow` for code folding
- `quelpa-use-package` for package installation and configuration
- Tuned startup, but emacsclient/server setup is recommended
- custom theme, that has dark and bright mode (toggle by key `C-c M-l`, powered by [heaven-and-hell](https://github.com/valignatev/heaven-and-hell))
- Note: some of the key mappings contain German umlauts
- Note: local copy of [hideshow.el](https://github.com/jwiegley/emacs-release/raw/master/lisp/progmodes/hideshow.el) and [dired-x.el](https://github.com/emacs-mirror/emacs/raw/master/lisp/dired-x.el) (cannot download file via emacs anymore)

## Keys

### Navigation
| Command               | Description                         |
| :---                  | :---                                |
| `C-x <Up/Down>`       | cycle windows                       |
| `M-j`                 | Moves cursor to left window         |
| `M-i`                 | ... upper window                    |
| `M-k`                 | ... lower window                    |
| `M-l`                 | ... right window                    |
| `C-x <Left/Right>`    | cycle buffers                       |
| `C-<PageDown/PageUp>` | cycle source files only             |
| `F6`                  | bs-show (opened sources files list) |
| `F5`                  | reload buffer                       |
| `C-M-<prior/next>`    | scroll-down/up row-wise             |
| `M-<PageDown/PageUp>` | scroll other window                 |
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
| `C-<Space> <Space>`   | set marker                          |
| `C-u <Space>`         | jump back                           |
| `C-x C-x`             | select marked region                |
| `C-x <Space>`         | rectangle selection mode            |
| `C-M <Up/Down>`       | go through level of parentheses     |
| `C-l`                 | center at position                  |
| `M-a`                 | backward-sentence                   |
| `C-v`                 | ivy/avy goto line                   |

### Org

| Command     | Description                                                           |
| :---        | :---                                                                  |
| `C-c a`     | org agenda                                                            |
| `C-c q`     | org agenda list                                                       |
| `C-c v`     | org capture                                                           |
| `C-c a v d` | create diary entry                                                    |
| `C-c C-c`   | run org action                                                        |
| `C-c -`     | org itemize                                                           |
| ...         | Alt+Enter, Alt+Shift+Right Subitem, Alt+Shift+Up, [Shift]-Tab Folding |
| `C-c l`     | store link                                                            |
|             |                                                                       |

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

### Coding

| Command | Description                                               |
| :---    | :---                                                      |
| `M-g o` | dumb-jump-go-other-window                                 |
| `M-g j` | dumb-jump-go                                              |
| `M-g i` | dumb-jump-go-prompt                                       |
| `M-m`   | start of code line                                        |
| `M-g k` | align code columns along regexp (like aligning along '=') |
| `C-;`   | comment/uncomment                                         |

### Python

| Command   | Description                                             |
| :---      | :---                                                    |
| `C-c C-c` | eval script or region                                   |
| `C-RET`   | eval current line plus the following nested lines).     |
| `C-c C-z` | switches between your script and the interactive shell. |
| `C-c C-d` | displays doc for the thing under cursor.                |

### Misc

| Command          | Description                                              |
| :---             | :---                                                     |
| `M-/`            | dabbrev-expand                                           |
| `M-r`            | line to bottom top                                       |
| `M-\`            | delete whitespaces                                       |
| `M-^`            | join to previous line                                    |
| `C-M-x`          | eval defun (lisp)                                        |
| `C-c M-l`        | toggle theme lightness                                   |
| `M-<digit>` + .. | repeats <digit>-times (`M-3 C-d` duplicates row 3 times) |

### Folding

| Command               | Description                         |
| :---                  | :---                                |
| `C-c s d`             | ha/hs-hide-all                      |
| `C-c s a`             | ha/hs-show-all                      |
| `C-c s s`             | ha/hs-toggle-hiding                 |

...

## Install

- Run recursive git clone for loading snippets:
```bash
git clone --recursive https://github.com/tdd11235813/emacs_config.git
```

## Misc

### Common Mistakes

- Do **not** use command-line argument to emacs init files. Do **not** use things like `-l init.el` for regular emacs start, it will mess up the init process.
See [emacs startup](https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html) and [after-init-hook issue 1](https://emacs.stackexchange.com/questions/51438/why-after-init-hook-is-not-invoked-workaround-is-emacs-startup-hook) and [after-init-hook issue 2](https://github.com/valignatev/heaven-and-hell/issues/11).
- Use `setq` in the custom theme. `custom-set-variables` does not seem to work there.

### Startup Time Measurement

``` bash
command time -p emacs -l $HOME/.emacs.d/init.el -Q -e kill-emacs
```

## Font

Font for coding:

``` bash
# from https://github.com/adobe-fonts/source-code-pro/issues/17
# ~/.fonts is now deprecated and that
#FONT_HOME=~/.fonts
# ~/.local/share/fonts should be used instead
FONT_HOME=~/.local/share/fonts

echo "installing fonts at $PWD to $FONT_HOME"
mkdir -p "$FONT_HOME/adobe-fonts/source-code-pro"
# find "$FONT_HOME" -iname '*.ttf' -exec echo '{}' \;

(git clone \
     --branch release \
     --depth 1 \
     'https://github.com/adobe-fonts/source-code-pro.git' \
     "$FONT_HOME/adobe-fonts/source-code-pro" && \
     fc-cache -f -v "$FONT_HOME/adobe-fonts/source-code-pro")
```

## Screenshots

Emacs running in terminal mode
![Screenshot Emacs in Terminal](/images/screenshot.jpg)

Emacs running in window mode (showing dumb-jump and neotree)
![Screenshot Emacs Window](/images/screenshot01.jpg)

## Used Packages

```bash
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
company
company-quickhelp
counsel
cuda-mode
delight
dired
dired+
dired-x
drag-stuff
dumb-diff
dumb-jump
duplicate-thing
ess-site
expand-region
flycheck
flyspell
general
git-gutter+
git-timemachine
glsl-mode
gnuplot-mode
hideshow
interleave
ivy
ivy-bibtex
ivy-hydra
ivy-yasnippet
json-mode
magit
markdown-mode
misc-cmds
modern-cpp-font-lock
multiple-cursors
neotree
nov
org
org-agenda
org-brain
org-cliplink
org-clock
org-clock-convenience
org-clock-today
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
recentf
reftex
saveplace
smex
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

# by themes

column-enforce-mode
cuda-mode
display-line-numbers
glsl-mode
highlight-numbers
linum
rainbow-delimiters
semantic/sb
spaceline-config
spacemacs-common

heaven-and-hell
```

## License

Public Domain (using [CC0](https://creativecommons.org/publicdomain/zero/1.0/)).

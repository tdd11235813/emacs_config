# Emacs config

## Features
- C++11 Support with code completion and refactoring features by rtags and cmake-ide
- improved, but still incomplete syntax highlighting (aimed for personal code style)
- CUDA, gnuplot, R

## Keys
- `M-ü` - speedbar
- `C-x <Up|Down>` - cycle windows
- `C-x <Left|Right>` - cycle buffers
- `C-<PageDown|PageUp>` - cycle source files only
- `F6` - bs-show (opened sources files list)
- `F5` - reload buffer
- `C-d` - duplicate line
- `C-c d` - kill line
- `M-f` - complete filename (company-files)
- `<backtab>` - code completion (company complete)
- `M-.` - find symbol/definition/.. at point (rtags)
- `M.,` - find references at point (rtags)
- `C-c ö` - switch to 2-space indentation style
- `C-c ä` - switch to 4-space indentation style
...

## Install
Run recursive git clone for loading submodules glsl-mode and ESS:
```
git clone --recursive https://github.com/tdd11235813/emacs_config.git
```
Run `make` in lisp/ESS.

rtags is required (which requires LLVM/Clang compiler):
- rtags `git clone --recursive https://github.com/Andersbakken/rtags.git`

For auto-installing further emacs packages, uncomment in init.el and init_scivis.el:
```
(setq use-package-always-ensure t)
```
For cmake-ide build-pool-dir is used (`cmake-ide-build-pool-dir "~/.cmake-ide/build"`).
For further hints see `init_cpp.el`.

## Emacs alias
C++ IDE, CUDA, ..
```
alias em="emacs -nw -l ~/.emacs.d/init.el"
alias ems="em --daemon=workspaceC"                                                                                                        
alias emc="emacsclient -nw -s workspaceC"
```
R (using ESS), gnuplot
```
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

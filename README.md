*Emacs config*

*Features*
- C++11 Support with code completion and refactoring features by rtags and cmake-ide
- improved, but still incomplete syntax highlighting (aimed for personal code style)
- CUDA, gnuplot, R

*Keys*
- `M-ü` - speedbar
- `C-<PageDown|PageUp>` - cycle source files only
- `C-x p` - plot gnuplot file

*Requirements*
- rtags `git clone --recursive https://github.com/Andersbakken/rtags.git`
- company-clang.el ( in lisp/ )
- glsl-mode.el ( in lisp/ )
- gnuplot-mode.el (in lisp/ ) # init_scivis.el
- ess ( in lisp/ ) # used for R support by init_scivis.el

*Setup*
`emacs -nw -l ~/.emacs.d/init.el` 

cd ~/.emacs.d/lisp/
wget -N https://github.com/emacsmirror/emacswiki.org/raw/master/misc-cmds.el
wget -N https://github.com/emacsmirror/dired-plus/raw/master/dired%2B.el
wget -N https://github.com/jwiegley/emacs-release/raw/master/lisp/progmodes/hideshow.el
wget -N https://github.com/emacs-mirror/emacs/raw/master/lisp/dired-x.el

emacs -l /home/mwerner/.emacs.d/init.el

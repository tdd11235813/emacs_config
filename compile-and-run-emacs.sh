EMACSVER=26.1
PORTABLE=$PWD/$EMACSVER

if [ "$1" == "install" ]; then

    echo Install to $PORTABLE

    if [ ! -f emacs-${EMACSVER}.tar.xz ]; then
        #wget -N
        curl https://mirror.checkdomain.de/gnu/emacs/emacs-${EMACSVER}.tar.xz -o emacs-${EMACSVER}.tar.xz
    fi
    mkdir -p /tmp/emacs-${EMACSVER}
    tar xf emacs-${EMACSVER}.tar.xz -C /tmp/

    cd /tmp/emacs-${EMACSVER}

    ./autogen.sh
    ./configure --prefix=$PORTABLE --without-all --without-x CFLAGS="-O3 -g -I$PORTABLE/include" #LDFLAGS="-L$PORTABLE/lib -Wl,-rpath=\\\$\$ORIGIN/../lib"
    make -j$(nproc) install

    # init.el
    if [ ! -d ~/.emacs.d/ ]; then
        git clone --recursive https://github.com/tdd11235813/emacs_config ~/.emacs.d
        cd ~/.emacs.d
        sh bootstrap.sh
    fi

fi

# run.sh
${EMACSVER}/bin/emacs -nw -l ~/.emacs.d/init.el

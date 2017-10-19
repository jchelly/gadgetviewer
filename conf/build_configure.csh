#!/bin/tcsh
#
# Rebuild the configure script
#

# charon
setenv GTK_PATH /usr/
setenv AUTOCONF_PATH /usr/
#setenv PATH /gpfs/data/jch/Software/autoconf/bin/:$PATH


rehash

cd ..
rm ./conf/config.guess
rm ./conf/config.sub
aclocal -I m4 -I ${GTK_PATH}/share/aclocal -I ${AUTOCONF_PATH}/share/aclocal
libtoolize --force --copy --install
aclocal -I m4 -I ${GTK_PATH}/share/aclocal -I ${AUTOCONF_PATH}/share/aclocal
autoheader
automake --add-missing --force-missing --copy
autoconf --force

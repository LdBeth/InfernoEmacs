# -*- mode: mkfile -*-
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$EMDIR
DUMP_EL=~/.emacs.d/dump.el

all:VQ:
	echo "Run mk redump."

redump:V:
	$EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" --batch --load $DUMP_EL
	mv ~/.emacs.d/Emacs.pdmp "${EMDUMPDIR}/Emacs.pdmp"


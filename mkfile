# -*- mode: mkfile -*-
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$EMDIR
SCPT_DIR=~/.emacs.d/script

all:VQ:
	echo "Run mk redump."

redump:V:
	$EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" -q --batch --load $SCPT_DIR/dump.el
	mv ~/.emacs.d/Emacs.pdmp "${EMDUMPDIR}/Emacs.pdmp"

split:V:
	$EMDIR/Emacs --batch --load $SCIP_DIR/split.el


# -*- mode: mkfile -*-
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$EMDIR/libexec/aarch64-apple-darwin24.1.0
SCPT_DIR=~/.emacs.d/script

all:VQ:
	echo "Run mk redump|split."

redump:V:
	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" -q --batch --load $SCPT_DIR/dump.el
	mv ~/.emacs.d/Emacs.pdmp "${EMDUMPDIR}/Emacs.pdmp"

orig:V:
	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak"

split:V:
	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" --batch --load $SCPT_DIR/split.el


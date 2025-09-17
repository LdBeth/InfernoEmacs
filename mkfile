# -*- mode: mkfile -*-
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$EMDIR/libexec
SCPT_DIR=~/.emacs.d/script

all:VQ:
	echo "Run mk redump|split."

# $EMDUMPDIR/Emacs.pdmp.bak:Psh -c 'test -f "$0"': $EMDUMPDIR/Emacs.pdmp
# 	cp $prereq $target

# redump:V: $EMDUMPDIR/Emacs.pdmp.bak
# 	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" -q --batch --load $SCPT_DIR/dump.el
# 	mv ~/.emacs.d/Emacs.pdmp "${EMDUMPDIR}/Emacs.pdmp"
redump:VQ:
	echo "Native lisp cannot redump"

orig:V:
	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak"

split:V:
	u $EMDIR/Emacs --dump "${EMDUMPDIR}/Emacs.pdmp.bak" --batch --load $SCPT_DIR/split.el


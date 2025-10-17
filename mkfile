# -*- mode: mkfile -*-
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$EMDIR/libexec
SCPT_DIR=~/.emacs.d/script
DEFSGEN=$EMDIR/Emacs -q --batch -f loaddefs-generate-batch
ORIGDMP=--dump $EMDUMPDIR/Emacs.pdmp.bak
all:VQ:
	echo "Run mk redump|split."

$EMDUMPDIR/Emacs.pdmp.bak:Psh -c 'test -f "$0"': $EMDUMPDIR/Emacs.pdmp
	cp $prereq $target

redump:V: $EMDUMPDIR/Emacs.pdmp.bak
	u $EMDIR/Emacs $ORIGDMP -q --batch --load $SCPT_DIR/dump.el
	mv ~/.emacs.d/Emacs.pdmp "${EMDUMPDIR}/Emacs.pdmp"

orig:V:
	u $EMDIR/Emacs $ORIGDMP

split:V:
	u $EMDIR/Emacs $ORIGDMP --batch --load $SCPT_DIR/split.el


autoloads:V: core/core-autoloads.el core/spacemacs-theme/spacemacs-theme-autoloads.el

%-autoloads.el:U:
	u $DEFSGEN $target `dirname $target`

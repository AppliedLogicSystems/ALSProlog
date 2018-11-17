#!/bin/sh
#	Applies pandoc to single file: md_help/$1.md => alshelp/$1.html
#
for ff in md_help/*.md; do
	ff=${ff##*/}
	ff=${ff%.*}
	MDF=md_help/$ff.md
	HTMLF=alshelp/$ff.html
	if [ -e $MDF ]
	then
		echo "pandoc -s -o html_help/$HTMLF $MDF"
		pandoc -s -o $HTMLF $MDF
	else
		echo "$MDF does not exist"
	fi
done

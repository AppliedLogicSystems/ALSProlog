#!/bin/sh
#	Applies pandoc to single file: md_help/$1.md => alshelp/$1.html
#
if [ -z "$1" ]
then
      echo "\$1 is empty"
else
	MDF=md_help/$1.md
	HTMLF=alshelp/$1.html
	if [ -e $MDF ]
	then
		echo "pandoc -s -o html_help/$HTMLF $MDF"
		pandoc -s -o $HTMLF $MDF
	else
		echo "$MDF does not exist"
	fi
fi

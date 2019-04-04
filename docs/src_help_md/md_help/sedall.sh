#!/bin/sh
#
for f in *.md; do sed -i.'' -f fixup.sed "$f"; done;

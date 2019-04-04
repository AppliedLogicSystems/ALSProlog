#!/bin/bash

# Builds a local version of docs

set -eux

cd "$(dirname "$0")/_local_site"

# Replace top level with docs
shopt -s extglob
rm -r !(assets|docs)
mv docs/* .

# "Sed is the last refuge of a link-breaking scoundrel."

if [ $(uname) == Darwin ]
then
	SED=(sed -i '')
else
	SED=(sed -i)
fi

# Strip /docs/ from links
"${SED[@]}" 's/href="\/docs\//href="\//' *.html */*.html
"${SED[@]}" 's/src="\/docs\//src="\//' *.html */*.html

# Make absolute links relative
"${SED[@]}" 's/href="\//href=".\//' *.html
"${SED[@]}" 's/href="\//href="..\//' */*.html

# Fix relative index links
"${SED[@]}" 's/href="\(..*\)\/"/href="\1\/index.html"/' *.html */*.html
"${SED[@]}" 's/href="\(..*\)\/#\(..*\)"/href="\1\/index.html#\2"/' *.html */*.html

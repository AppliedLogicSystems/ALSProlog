#!/bin/bash

# Builds a local version of docs

set -eu

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
"${SED[@]}" 's/href="\/docs\//href="\//g ; s/src="\/docs\//src="\//g' *.html */*.html

# Make absolute links relative
"${SED[@]}" 's/href="\//href=".\//g ; s/src="\//src=".\//g' *.html
"${SED[@]}" 's/href="\//href="..\//g ; s/src="\//src="..\//g' */*.html

# Fix relative index links
"${SED[@]}" 's/href="\(.[^"]*\)\/"/href="\1\/index.html"/g' *.html */*.html
"${SED[@]}" 's/href="\(.[^"]*\)\/#\(.[^"]*\)"/href="\1\/index.html#\2"/g' *.html */*.html

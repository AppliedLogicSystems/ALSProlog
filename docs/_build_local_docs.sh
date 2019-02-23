#!/bin/bash

# Builds a local version of docs

set -eux

cd "$(dirname "$0")"

${BUNDLE:-bundle} exec jekyll build --config _config.yml,_local_config.yml

pushd _local_site

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

# Fix root links
"${SED[@]}" 's/href="\/"/href=".\/index.html"/' *.html
"${SED[@]}" 's/href="\/"/href="..\/index.html"/' */*.html

# Fix asset links
"${SED[@]}" 's/href="\/assets\//href="assets\//' *.html
"${SED[@]}" 's/href="\/assets\//href="..\/assets\//' */*.html

# Fix Guide/Ref links
"${SED[@]}" 's/href="guide\/\(..*\)"/href="guide\/\1.html"/' *.html
"${SED[@]}" 's/href="guide\/"/href="guide\/index.html"/' *.html
"${SED[@]}" 's/href="ref\/"/href="ref\/index.html"/' *.html
"${SED[@]}" 's/href="ref\/#\(.*\)"/href="ref\/index.html#\1"/' *.html

# Fix links within Refs
"${SED[@]}" 's/href="\/docs\/ref\/\(.*\)"/href="\1"/' ref/*.html

# Fix links within Guide
"${SED[@]}" 's/href="\([^#\/]*\)"/href="\1.html"/' guide/*.html

# Fix Ref-Guide cross-links
"${SED[@]}" 's/href="\.\.\/guide\/\([^#\/]*\)"/href="..\/guide\/\1.html"/' ref/*.html

# Fix relative index links
"${SED[@]}" 's/href="\(..*\)\/"/href="\1\/index.html"/' *.html */*.html
"${SED[@]}" 's/href="\(..*\)\/#\(..*\)"/href="\1\/index.html#\2"/' *.html */*.html


# Fix images links
"${SED[@]}" 's/src="\/docs\/[^\/]*\//src="/' */*.html


popd

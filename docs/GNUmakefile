.DEFAULT_GOAL=help

.DELETE_ON_ERROR:

ifneq (,$(findstring CYGWIN, $(shell uname)))
  BUNDLE = bundle.bat
else
  BUNDLE = bundle
endif

source = $(shell find . -type f -not -name Gemfile.lock -not -path './_site/*' -not -path './_local_site/*' -not -name .DS_Store)

###########################################
.PHONY : all site local run

## Build site and local docs
all : site local

## Run site for live development
run : Gemfile.lock
	$(BUNDLE) exec jekyll serve --incremental --livereload --open-url

## Build site in _site
site : _site/index.html

## Build local docs in _local_site
local : _local_site/index.html

Gemfile.lock : Gemfile GNUmakefile
	rm -f Gemfile.lock
	$(BUNDLE) install

_site/index.html : Gemfile.lock $(source)
	$(BUNDLE) exec jekyll build

_local_site/index.html : Gemfile.lock $(source)
	$(BUNDLE) exec jekyll build --config _config.yml,_local_config.yml
	./_build_local_docs.sh

###########################################
.PHONY : test test-site test-local test-unused

## Test all markup and links
test : test-site test-local

## Test site
test-site : site
	$(BUNDLE) exec htmlproofer \
	--internal-domains "alsprolog.com" \
	--check-html --check-img-http --enforce-https --check-opengraph --check-sri \
	--empty-alt-ignore _site

## Test local docs
test-local : local
	$(BUNDLE) exec htmlproofer \
	--directory-index-file noindex.html \
	--check-html --disable-external --empty-alt-ignore \
	_local_site

## Test for un-reachable site files (needs `bundle exec jekyll serve`)
test-unused :
	TEMPDIR=$$(mktemp -d) ; \
	wget --no-verbose --recursive --directory-prefix=$$TEMPDIR --no-host-directories http://127.0.0.1:4000 \
	&& diff -ru _site $$TEMPDIR

###########################################
.PHONY : clean help

## Clean all generated files
clean:
	rm -rf Gemfile.lock _site _local_site .jekyll-metadata

## Display this help
help:
	@echo Makefile Targets:
	@awk -F ':|##' \
	'/^##/ {d=$$2}; /^[^#]/ {if (d) {printf "\033[36m%-20s\033[0m %s\n",$$1,d}; d=""}' \
	$(MAKEFILE_LIST)

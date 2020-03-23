.DEFAULT_GOAL=help

.DELETE_ON_ERROR:

# Prevent passing custom CFLAGS, because it can interfere with Bundle builds.
unexport CFLAGS

ifneq (,$(findstring CYGWIN, $(shell uname)))
  BUNDLE = bundle.bat
else
  BUNDLE = bundle
endif

source = $(shell find . -not -name Gemfile.lock -not -path './_site*' -not -path './_local_site*' -not -name .DS_Store)

local_config = --config _config.yml,_local_config.yml

###########################################
.PHONY : all site local run

## Build site and local docs
all : site local

## Run site for live development
run : Gemfile.lock
	# Note: index is deleted after serving to ensure site builds correctly
	$(BUNDLE) exec jekyll serve --incremental --livereload --open-url ; rm -f _site/index.html

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
	$(BUNDLE) exec jekyll build $(local_config)
	./_build_local_docs.sh

###########################################
.PHONY : test test-site test-local test-unused

## Test all markup and links
test : test-site test-local

## Test site
test-site : site
	$(BUNDLE) exec jekyll doctor
	$(BUNDLE) exec htmlproofer \
	--internal-domains "alsprolog.com" \
	--check-html --check-img-http --enforce-https --check-opengraph --check-sri \
	--check-external-hash --empty-alt-ignore _site

## Test local docs
test-local : local
	$(BUNDLE) exec jekyll doctor $(local_config)
	$(BUNDLE) exec htmlproofer \
	--directory-index-file noindex.html \
	--url-swap '^/:noabsolute' \
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
	$(BUNDLE) exec jekyll clean
	$(BUNDLE) exec jekyll clean $(local_config)
	rm -rf Gemfile.lock

## Display this help
help:
	@echo Makefile Targets:
	@awk -F ':|##' \
	'/^##/ {d=$$2}; /^[^#]/ {if (d) {printf "\033[36m%-20s\033[0m %s\n",$$1,d}; d=""}' \
	$(MAKEFILE_LIST)

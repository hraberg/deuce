SHELL=/bin/bash

emacs_version=24.2
emacs_tag=emacs-$(emacs_version)
emacs_configure_flags=--with-x=no --without-xpm --without-jpeg --without-tiff --without-gif --without-png \
	--without-toolkit-scroll-bars --without-xim --without-sound --without-makeinfo --without-selinux \
	--without-dbus --without-gsettings --without-gconf --without-gnutls # 24.3: ./configure --without-all --with-x=no
emacs_lisp_files=$(shell find emacs/lisp -iname "*.el")

zile_version=2.4.11
zile_tag=v$(zile_version)

deuce_version=$(shell head -1 project.clj | awk '{ print $$3 }' | sed s/\"//g)
deuce_uberjar=target/deuce-$(deuce_version)-standalone.jar
deuce_stubs=src-$(emacs_version)-stubs
deuce_source_files=$(shell find src -iname "*.clj")

deuce_javascript=$(shell find resources/public -iname "*.js" ! -path "*/node_modules/*")
deuce_css=$(shell find resources/public -iname "*.css")

nw_version=0.12.0-alpha2
node_modules=resources/public/node_modules
nwbuild=node $(node_modules)/.bin/nwbuild -v $(nw_version)
nw_tests=$(shell grep -l "require('assert')" $(deuce_javascript))

smoke_test_args=-Q --batch --eval "(print (emacs-version))"

all: target/deuce test

emacs/Makefile:
	git submodule update --init emacs
	(cd emacs && git checkout $(emacs_tag) && ./autogen.sh && ./configure $(emacs_configure_flags))

emacs/src/emacs: emacs/Makefile
	make -C emacs CONFIGURE_FLAGS="$(emacs_configure_flags)" bootstrap

emacs/src/temacs: emacs/src/emacs
	make -C emacs/src temacs

emacs/lisp/loaddefs.el: emacs/src/emacs
	make -C emacs/lisp autoloads

emacs-tests: emacs/src/emacs
	make -C emacs/test/automated check

emacs-smoke: emacs/src/temacs
	$< $(smoke_test_args)

zile/Makefile:
	git submodule update --init zile
	(cd zile && git checkout $(zile_tag) && ./bootstrap && ./configure)

zile/src/zile: zile/Makefile
	make -C zile

zile-tests: zile/src/zile emacs/src/emacs
	make -C zile check EMACSPROG=../emacs/src/emacs

$(deuce_uberjar): emacs/lisp/loaddefs.el $(emacs_lisp_files) $(deuce_source_files)
	lein do run -q --batch, uberjar

define deuce_header
#!/bin/sh
exec java -client -noverify -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -jar "$$0" -q "$$@"
exit
endef
export deuce_header

target/deuce: $(deuce_uberjar)
	echo "$$deuce_header" > $@
	cat $(deuce_uberjar) >> $@
	chmod +x $@

clean:
	rm -rf target

dist: clean all

test:
	lein difftest

smoke: target/deuce
	$< $(smoke_test_args)

$(node_modules): resources/public/package.json
	(cd $(@D) && npm install)
	touch $@

jslint: $(deuce_javascript) $(node_modules)
	$(node_modules)/.bin/$@ $?

csslint: $(deuce_css) $(node_modules)
	$(node_modules)/.bin/$@ --ignore=adjoining-classes $?

lint: jslint csslint

nw: $(node_modules)
	$(nwbuild) -r resources/public

nwbuild: $(node_modules)
	$(nwbuild) -p linux64,osx64,win64 -o target/nwbuild resources/public

nwtest: jslint
	$(foreach f, $(nw_tests),node $(f);)

run: target/deuce
	$<

run-dev: emacs/lisp/loaddefs.el
	lein run -Q --nrepl || reset

docs/index.html: $(deuce_source_files)
	git submodule update --init docs
	rm -rf docs/codox
	lein do marg-el, doc

docs: docs/index.html

emacs/src/TAGS: emacs/Makefile
	make -C emacs/src tags

emacs/src/TAGS-LISP: emacs/src/TAGS

emacs/src/TAGS-TEMACS: emacs/src/temacs
	(cd emacs/src && \
		../lib-src/etags -o TAGS-TEMACS --regex='/[     ]*DEFVAR_[A-Z_  (]+"\([^"]+\)"/' $$(echo *.o | sed s/\\.o/.c/g))

TAGS: emacs/src/TAGS emacs/src/TAGS-LISP
	cat $? > TAGS
	find src/deuce/emacs/ -name "*.clj" -print | etags --language=lisp -a TAGS -

$(deuce_stubs): emacs/src/TAGS-TEMACS
	@echo "NOTE: this doesn't take the interactive directivies on the DEFUN into account. It probably should."
	lein run -m deuce.scaffold $(deuce_stubs)/deuce/emacs

stubs: $(deuce_stubs)

.PHONY: test emacs-tests zile-tests emacs-smoke smoke clean stubs dist run run-dev all jslint csslint lint nw nwbuild nwtest

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

all: target/deuce test

emacs/Makefile:
	git submodule update --init emacs
	(cd emacs && git checkout $(emacs_tag) && ./autogen.sh && ./configure $(emacs_configure_flags))

emacs/src/bootstrap-emacs: emacs/Makefile
	make -C emacs CONFIGURE_FLAGS="$(emacs_configure_flags)" bootstrap

emacs/src/temacs: emacs/src/bootstrap-emacs
	make -C emacs/src temacs

emacs/lisp/loaddefs.el: emacs/src/bootstrap-emacs
	make -C emacs/lisp autoloads

emacs-tests: emacs/src/bootstrap-emacs
	(cd emacs/test/automated && make check)

smoke: emacs/src/temacs
	./emacs/src/temacs -Q --batch --eval "(print (emacs-version))"

zile/Makefile:
	git submodule update --init zile
	(cd zile && git checkout $(zile_tag) && ./bootstrap && ./configure)

zile/src/zile: zile/Makefile
	(cd zile && make)

zile-tests: zile/src/zile
	(cd zile && make check)

$(deuce_uberjar): emacs/lisp/loaddefs.el $(emacs_lisp_files) $(deuce_source_files)
	lein do run -q --batch, uberjar

test:
	lein difftest

clean:
	rm -rf target

define deuce_header
#!/bin/sh
exec java -client -noverify -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -jar "$$0" -q "$$@"
exit
endef
export deuce_header

target/deuce: $(deuce_uberjar)
	echo "$$deuce_header" > target/deuce
	cat $(deuce_uberjar) >> target/deuce
	chmod +x target/deuce

dist: clean all

run: target/deuce
	target/deuce

run-dev: emacs/lisp/loaddefs.el
	lein run -Q --nrepl || reset

emacs/src/TAGS: emacs/Makefile
	make -C emacs/src tags

emacs/src/TAGS-LISP: emacs/src/TAGS

emacs/src/TAGS-TEMACS: emacs/src/temacs
	(cd emacs/src && \
		../lib-src/etags -o TAGS-TEMACS --regex='/[     ]*DEFVAR_[A-Z_  (]+"\([^"]+\)"/' $$(echo *.o | sed s/\\.o/.c/g))

TAGS: emacs/src/TAGS emacs/src/TAGS-LISP
	cat emacs/src/TAGS* > TAGS
	find src/deuce/emacs/ -name "*.clj" -print | etags --language=lisp -a TAGS -

$(deuce_stubs): emacs/src/TAGS-TEMACS
	@echo "NOTE: this doesn't take the interactive directivies on the DEFUN into account. It probably should."
	lein run -m deuce.scaffold $(deuce_stubs)/deuce/emacs

stubs: $(deuce_stubs)

.PHONY: test emacs-tests zile-tests smoke clean stubs dist run run-dev all

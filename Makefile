SHELL=/bin/bash

emacs_version=24.2
emacs_tag=emacs-$(emacs_version)
emacs_configure_flags=  --with-x=no --without-xpm --without-jpeg --without-tiff --without-gif --without-png \
	--without-toolkit-scroll-bars --without-xim --without-sound --without-makeinfo --without-selinux \
	--without-dbus --without-gsettings --without-gconf --without-gnutls

deuce_version=$(shell head -1 project.clj | awk '{ print $$3 }' | sed s/\"//g)
deuce_uberjar=target/deuce-$(deuce_version)-standalone.jar
deuce_stubs=src-$(emacs_version)-stubs

all: dist

# 24.3: ./configure --without-all --with-x=no
emacs/Makefile:
	git submodule update --init emacs
	(cd emacs && git checkout $(emacs_tag) && ./autogen.sh)
	(cd emacs && ./configure $(emacs_configure_flags))

emacs/src/bootstrap-emacs: emacs/Makefile
	make -C emacs CONFIGURE_FLAGS="$(emacs_configure_flags)" bootstrap

emacs/src/temacs: emacs/src/Makefile
	make -C emacs/src temacs

emacs/lisp/loaddefs.el: emacs/src/temacs
	make -C emacs/lisp autoloads

smoke: emacs/src/temacs
	./emacs/src/temacs -Q --batch --eval "(print (emacs-version))"

emacs/src/TAGS: emacs/Makefile
	make -C emacs/src tags

emacs/src/TAGS-LISP: emacs/src/TAGS

emacs/src/TAGS-TEMACS: emacs/src/temacs
	(cd emacs/src && \
		../lib-src/etags -o TAGS-TEMACS --regex='/[     ]*DEFVAR_[A-Z_  (]+"\([^"]+\)"/' $$(echo *.o | sed s/\\.o/.c/g))

TAGS: emacs/src/TAGS emacs/src/TAGS-LISP
	cat emacs/src/TAGS* > TAGS
	find src/deuce/emacs/ -name "*.clj" -print | etags --language=lisp -a TAGS -

$(deuce_uberjar): emacs/src/temacs emacs/lisp/loaddefs.el src
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

dist: clean $(deuce_uberjar) test target/deuce

run: target/deuce
	target/deuce

$(deuce_stubs): emacs/src/TAGS-TEMACS
	@echo "NOTE: this doesn't take the interactive directivies on the DEFUN into account. It probably should."
	lein run -m deuce.scaffold $(deuce_stubs)/deuce/emacs

stubs: $(deuce_stubs)

.PHONY: test smoke clean stubs dist run all

# DEUCE - Deuce is Emacs under Clojure

*Because it's there* -- George Mallory


**Note: Absolutely NOTHING works yet. Expect until Q3 2012 before anything even remotely interesting.**


### Preparing Emacs

The target version of Emacs is 23.4. It's assumed to live under `emacs`. `configure-emacs` will download it if not.


For a minimal [Emacs build](http://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.html):

    ./configure-emacs # downloads emacs-23.4.tar.bz if needed
    ./make-emacs temacs

    ./emacs/src/temacs -Q --batch --eval "(print (emacs-version))" # ./smoke

`temacs` is "bare impure Emacs", the raw C version of Emacs, without any Emacs Lisp AOTd.
The Emacs Lisp lives under `emacs/lisp`. `-Q` is "-q --no-site-file --no-splash", it basically surpresses all customizations. `--batch` won't open the display editor.

The above should output:

    Loading loadup.el (source)...
    Using load-path (<path-to>/emacs/lisp)
    Loading emacs-lisp/byte-run...
    [... loads of Emacs Lisp loaded ...]

    "GNU Emacs 24.1.50 (x86_64-unknown-linux-gnu)
    of 2012-04-13 on hraberg-VPCZ21C5E"


*The task at hand is to get rid of the bare impure Emacs, replace it with Clojure and the JVM, while keeping Emacs Lisp running.*

Clojure will be a first class citizen along Emacs Lisp in this new world. There may be ways to get this build even smaller, haven't looked into it yet.


### Emacs Lisp to Clojure

There are several issues (like dynamic scoping), but nothing too hard or exciting. This layer will work similar to [shen.clj](https://github.com/hraberg/shen.clj), that is, basically a simple source to source transformer between Emacs Lisp and Clojure. Emacs Lisp bytecode, and anything related to evaluation of Emacs Lisp in bare Emacs will simply be replaced with Clojure and not ported. Emacs Lisp is a more complex language than K Lambda (which underpins Shen) though, which also was designed specifically for porting.


### C to Clojure

A large part of bare Emacs is pretty redundant in 2012, so this will be mapped to JVM languages, and exposed to Emacs Lisp as the same primitives it has come to know and love. A subset of the Emacs C code is dealing with buffers, regex and other editing specifics, which will be harder to just replace.

Bare impure Emacs is 203692 lines of C spread over 65 files and another 19912 lines of header files. There are around 1064 primitve `defsubr` in the minimal build.

To simplify introspection, there are [BridJ](http://code.google.com/p/bridj/) stubs for the Emacs header files, which live under `src/emacs`:

    # Doesn't work, the stubs are generated, but cannot be compiled.
    ./build-stubs

These stubs aren't intended to be used to actually call Emacs natively, but for quick and dirty REPL access to the structure of the C code and scaffolding.

The actual porting of the C will be done using a tactic of avoidance until a function is needed, auto generation of its signatures second, and hand crafting the actual implementation last.

[`etrace`](http://ndevilla.free.fr/etrace/) can be linked to Emacs and when compiling with `-finstrument-functions` to get a crazy amount of tracing "insight" into what Emacs is doing. [`strace`](http://sourceforge.net/projects/strace/) is another alternative to see what Emacs is doing system call-wise, like to simply see just what files it opens.


### The Editor

I don't expect the visual editor to exist for quite a while. Initially, the editor itself will be implemented using [Charva](http://www.pitman.co.za/projects/charva/index.html) (or similar) Java curses/console library to keep things as simple as possible, compability wise. Eventually Swing, SWT and browser based front ends can be added to the mix.


### Testing

Larger than the technical challanges - which are mainly about scale - is the fact it doesn't seem to be any large regression suite for Emacs one can use to ensure one is on the right track. There are some tests, and other editors, like Zile, have Emacs compability test suites for at least editing that could be reused.


### The Road Map

My guess is that it will take roughly a month to get anyhting useful at all out of batch mode with basic Emacs Lisp cross compilation. An editor that can do anything but crashing another 2 months. An actual useful, somewhat compatbile subset of Emacs 6 months.

Matching the performance and exect characteristics of the C code for buffers etc. isn't a goal.

100% compatbility is never excpected, as the port needs to be driven by the need to support a useful, growing subset of Emacs Lisp packages and Emacs features.


### Future

Once Emacs works again, we can move it forward into the future, where it originally came from. I eventually envision something quite different from Emacs, but that may very well end up being another project all together. But having a useful subset of Emacs running on the JVM may come handy when one least excepts it.

The real goal is to bring back some of the fun of extending one's programming envioronment, by removing some of the old constraints and open up new possibilites - while respecting the Emacs tradition.


## References

[EMACS: The Extensible, Customizable Display Editor](http://www.gnu.org/software/emacs/emacs-paper.html) Richard Stallman, 1981

[GNU Emacs Lisp Reference Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)

[Emacs and Common Lisp](http://tromey.com/blog/?p=709) Tom Tromey, 2012

[Emacs Lisp in Edwin Scheme](http://www.dtic.mil/dtic/tr/fulltext/u2/a276721.pdf) Matthew Birkholz, 1993

[The Craft of Text Editing: Emacs for the Modern World](http://www.finseth.com/craft/) Craig A. Finseth, 1991, 1999

[Climacs - An Emacs-like editor in Common Lisp](http://common-lisp.net/project/climacs/)

[Portable Hemlock](http://www.common-lisp.net/project/phemlock/) Hemlock is another Common Lisp Emacs.

[Zile](http://www.gnu.org/software/zile/) "Zile is lossy Emacs"

[uemacs](http://git.kernel.org/?p=editors/uemacs/uemacs.git;a=tree) Linus' micro Emacs.

[JEmacs - The Java/Scheme-based Emacs](http://per.bothner.com/papers/Freenix00/Freenix00.html) Per Bothner, 2000


#### This is NOT YET GNU Emacs, one component of the GNU/Linux operating system.


## License

[GNU General Public License Version 3](http://www.gnu.org/licenses/)

# DEUCE - Deuce is (not yet) Emacs under Clojure

*Because it's there* -- George Mallory


**Note: Absolutely NOTHING works yet. Expect until Q1 2013 before anything even remotely interesting.**

Also - there's a risk I'll give up, far before reaching the current benchmark of JVM Emacsen: [JEmacs](http://per.bothner.com/papers/Freenix00/Freenix00.html).

### What is there to see?

[Marginalia](http://ghettojedi.org/deuce/)

**2013-02-28**

The new evaluation model is in. This creates a `.clj` file for each `.el` file it loads, which can then be compiled into a `.class` file. The use of Clojure's `eval` is gone, and the dynamic scoping is now all done by vars (which may be accessed via lexically). The core is also vastly simplified, it had many defensive post-walks for opaque root causes which the slow load time made very hard (for the impatient) to debug.

While the initial compilation actually is slower, it opens up for a better development model overall, as you can actually see and reevaluate the generated Clojure while solving an issue (and get proper line numbers), and once compiled, it loads much faster - `lein uberjar` can now basically be used to create an Emacs which isn't "bare".

Also new is [Bjarte](https://github.com/ljos)'s work on proper Cons lists, which allows for proper (but insane) semantics of destructive updates on mutually shared cells. `deuce-loadup` has slipped back a bit, but now I can refocus on the boot. As mentioned below, I will likely move to a more hand coded version of `loadup` which skips cumbersome parts of dubious immediate value of the full Emacs boot.

    $ lein run --batch --eval "(print (emacs-version))"

    Loading deuce-loadup.el...
    Using load-path ("" "emacs-lisp" "language" "international" "textmodes")
    Loading emacs-lisp/byte-run...
    Loading emacs-lisp/backquote...
    Loading subr...
    Loading version.el...
    GNU Emacs 24.2 (jvm-1.7.0_13_clojure-1.5.0-RC17)
     of 2013-02-28 on X202E


I've managed established a hacking base in Agonda now. No sleep until `M-x butterfly`.


**2013-02-25**

After a long break from Deuce, I'm now in Agonda, Goa, specifically with the aim to hack on Deuce for two months. Thanks to [Bjarte](https://github.com/ljos) and [Navgeet](https://github.com/Navgeet) for their pull requests, despite my lack of focus here.

I plan to start with a rethink of how the Emacs Lisp evaluation model works - it's currently way too slow, and relies heavily on Clojure's `eval`. I'm also considering to just ditch or stub out some parts of `loadup` (specifically `mule`) for now and aim to get the editor actually starting in some form.


**2012-11-08**

After many false starts, [`cl.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/cl.el) now loads. I don't expect much of it to actually work, but some things do. See the [EmacsWiki](http://www.emacswiki.org/emacs/CommonLispForEmacs) [CommonLispForEmacs](http://www.emacswiki.org/emacs/CommonLispForEmacs) page for a discussion about when this file is actually supposed to be loaded.

> Packages distributed within GNU Emacs don’t use cl at runtime

Instead files like [`env.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/env.el) use

```el
(eval-when-compile (require 'cl))
```

The documentation to `eval-when-compile` says

> In interpreted code, this is entirely equivalent to `progn'

Compiling Emacs Lisp to Clojure counts as interpreting in Deuce - but anything in [`loadup.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/loadup.el) is normally already compiled when starting Emacs. When running Emacs with `-Q` you can see that `(featurep 'cl)` actually is `nil`, so in theory `cl.el` shouldn't be loaded, but this distinction is messy as things can depend on the `cl.el` macros. For obvious reasons it would be nice to avoid it, as we have a situation where Deuce contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp in an ad hoc, informally-specified, bug-ridden, slow implementation of half of Emacs Lisp in Clojure.

Next up is [`bindings.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/bindings.el) which is its own area of strangeness, but at least something related to the actual editor.


**2012-10-09**

Deuce is not dead. I've been to the US past few weeks, reflecting on StrangeLoop - hacking on a few new things.
I'm now back in London, gearing up to tackle the method length issue, see below.

**2012-09-21**

I've been working on [Shen.java](https://github.com/hraberg/Shen.java) last week, so no progress here.
Currently [`cl-macs.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/cl-macs.el) fails with `ClassFormatError: Invalid method Code length` in `cl-parse-loop-clause`.

**2012-09-11 Logging**

Using [Timbre, a (sane) logging library for Clojure](https://github.com/ptaoussanis/timbre) (Taoussanis, 2012).

**2012-09-10 Mutable Data**

Somewhat reluctantly now backing Emacs Lisp with [LinkedList](http://docs.oracle.com/javase/7/docs/api/java/util/LinkedList.html) for [lists](http://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html) and arrays for [vectors](http://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html), so various destructive updates work. Both [`custom.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/custom.el) and [`mule.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/international/mule.el) are using this to build up various state. I've also decided to represent property lists as meta data on the var for now.

Here's the current state - [`load-path`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Loading.html) entries are classpath relative (from [`emacs/lisp`](https://github.com/emacsmirror/emacs/tree/master/lisp), see [`project.clj`](https://github.com/hraberg/deuce/blob/master/project.clj)):


    $ lein run --batch --eval "(print (emacs-version))"

    Loading deuce-loadup.el...
    Using load-path ("" "emacs-lisp" "language" "international" "textmodes")
    Loading emacs-lisp/byte-run...
    Loading emacs-lisp/backquote...
    Loading subr...
    Loading version.el...
    Loading widget...
    Loading custom...
    Loading emacs-lisp/map-ynp...
    Loading cus-start...
    Loading international/mule...
    GNU Emacs 24.2 (jvm-1.8.0-ea_clojure-1.4.0)
     of 2012-09-10 on hraberg-VPCZ21C5E



**2012-09-07** A new section about [hacking on Deuce](https://github.com/hraberg/deuce#hacking).

**Tentative Goals for September 2012**

* Getting [`subr.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/subr.el) to load properly (and then continue [`loadup.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/loadup.el)).
* Proper loading of Emacs Lisp in general.
* Proper handling of lexical vs. dynamic scoping.
* Getting the [`ert.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/ert.el) [`ert-tests.el`](https://github.com/hraberg/deuce/blob/master/test/ert-tests.el) self tests running. This will require some buffer variable handling. [ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html) also requires a few other files to work (like [`cl.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/cl.el)).

**2012-09-04 Deuce-Loadup**

    $ java -jar target/deuce-0.1.0-SNAPSHOT-standalone.jar --batch --eval "(print (emacs-version))"
    Loading deuce-loadup.el...
    Using load-path ("target/deuce-0.1.0-SNAPSHOT-standalone.jar")
    Loading emacs-lisp/byte-run...
    Loading emacs-lisp/backquote...
    Loading subr...
    Loading version.el...
    GNU Emacs 24.2 (jvm-1.8.0-ea_clojure-1.4.0)
     of 2012-09-04 on hraberg-VPCZ21C5E

[`deuce-loadup.el`](https://github.com/hraberg/deuce/blob/master/src/deuce-loadup.el) represents a small fraction of the real Emacs [`loadup.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/loadup.el). Not all forms loaded are even properly evaluated yet, but we get to [`version.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/version.el).

You can also run it like this:

    lein run --batch --eval "(print (emacs-version))"

You can evaluate arbitrary Emacs Lisp using `--eval` (most things won't work). `--script` can be used to load a `.el` file from disk.
`--batch` is currently a mandatory parameter, to represent the fact that there's no real display editor to load yet.


**2012-08-25 The [clojure-lanterna](https://github.com/sjl/clojure-lanterna/) screen test**

    lein trampoline run -m deuce.test.terminal

This is *NOT* Emacs, Deuce or anything close like it, even though it looks like it at first sight.
It is meant to ensure that Lanterna can handle the UI updates Emacs requires.

[`terminal.cjl`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/terminal.clj).


**2012-08-23 Emacs Lisp**

The Clojure macros that backs Emacs Lisp lives in [`deuce.emacs-lisp`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp.clj).
The exact evaluation model haven't been decided, but as Emacs Lisp is a Lisp 2, there will be a namespace for functions and one for variabels (currently called `deuce.emacs-lisp.globals`).

[`dynamic.cjl`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/dynamic.clj), [`lexical.clj`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/lexical.clj) and [`locals.clj`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/locals.clj) is a first stab at evaluating some Emacs Lisp. See [`deuce.test`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/) for more.

A test representing a tiny fraction of [`loadup.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/loadup.el), [`loadup.clj`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/loadup.clj).


**2012-04-22 The Emacs Lisp parser**

    lein run -m deuce.test.parser

It can read all the `.el` files under `emacs/lisp`, but the actual representation as Clojure forms will probably change.

[`parser.cjl`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp/parser.clj). This was one of the first things written back right before [EuroClojure](http://euroclojure.com/2012/), before I put Deuce on hold after admitting that the scope of this project required full time dedication.


### Preparing Emacs

The target version of Emacs is 24.2. It lives under `emacs` as a git submodule.

For a minimal [Emacs build](http://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.html):

    ./configure-emacs # Will clone emacs the first time (~1Gb).
                      # Might complain about missing `libtool`.
    make -C emacs # takes a few minutes, only needs to be run once, then you can use ./make-emacs.

    ./emacs/src/temacs -Q --batch --eval "(print (emacs-version))" # ./smoke

`temacs` is "bare impure Emacs", the raw C version of Emacs, without any Emacs Lisp preloaded.
The Emacs Lisp lives under `emacs/lisp`. `-Q` is `-q --no-site-file --no-splash`, it basically suppresses all customizations. `--batch` won't open the display editor.

The above should output:

    Loading loadup.el (source)...
    Using load-path (<path-to>/emacs/lisp)
    Loading emacs-lisp/byte-run...
    [... loads of Emacs Lisp loaded ...]

    "GNU Emacs 24.2 (x86_64-unknown-linux-gnu)
     of 2012-09-03 on hraberg-VPCZ21C5E"

*The task at hand is to get rid of the bare impure Emacs, replace it with Clojure and the JVM, while keeping Emacs Lisp running.*

Clojure will be a first class citizen along Emacs Lisp in this new world. There may be ways to get this build even smaller, haven't looked into it yet.


#### Tags

Run [`./collect-tags`](https://github.com/hraberg/deuce/blob/master/collect-tags) and add something like this to your `init.el`:

```el
;; To navigate between C and Emacs Lisp
(require 'etags-select)
(require 'etags-table)

(global-set-key "\M-." 'etags-select-find-tag)
(setq etags-table-search-up-depth 10)
```

There are probably better and cleaner ways of doing this, as TAGS includes TAGS-LISP (there's a hint at [here](http://www.emacswiki.org/emacs/EtagsSelect)).


### Hacking

While the most of the document below represents speculation about how the port might work, this discusses how the port *actually* works.

You need Emacs (see above) to have access to the Emacs Lisp, but actually building Emacs is optional (this is the entire point of Deuce).
I use Emacs to develop Deuce, which has a few recursive advantages - you can constantly verify how things should work in a real Lisp Interaction buffer.

After a few days of hacking, I found that it's easiest to load and switch to the [`deuce.emacs`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs.clj) and use this as your "base" namespace (`clojure.core` is required as `c`). Most actual hard work goes into [`deuce.emacs-lisp`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp.clj). The various namespaces under [`deuce.emacs`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs) replaces the [C core](https://github.com/emacsmirror/emacs/tree/master/src) of GNU Emacs.

Once in [`deuce.emacs`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs.clj) you can either evaluate Emacs Lisp as raw Clojure or via `eval` - the latter is more realistic as there's (often needed) processing done to the Clojure forms before they are really evaluated as Emacs Lisp. So normally do this:

```el
(eval '(setq x 2))
```

To try to load [`deuce-loadup.el`](https://github.com/hraberg/deuce/blob/master/src/deuce-loadup.el), do:

```el
(load "deuce-loadup")
```

You can also load other Emacs Lisp (from the class path) with:

```el
(load "subr")
```

Line numbers for Emacs Lisp are off a bit, but navigation with `M-.` from Clojure to Emacs Lisp should roughly work for `defun` and `defmacro`.

Tests in [`deuce.test`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/) are written in an REPL example style, most are ported from the [GNU Emacs Lisp Reference Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html), and are mainly targeting core features of Emacs Lisp, not the various library functions.


### Emacs Lisp to Clojure

There are several issues (like dynamic scoping), but nothing too hard or exciting. This layer will work similar to [shen.clj](https://github.com/hraberg/shen.clj), that is, basically a simple source to source transformer between Emacs Lisp and Clojure. Emacs Lisp bytecode, and anything related to evaluation of Emacs Lisp in bare Emacs will simply be replaced with Clojure and not ported. Emacs Lisp is a more complex language than K Lambda (which underpins Shen) though, which also was designed specifically for porting.

The special forms of Emacs Lisp live in [`deuce.emacs-lisp`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp.clj).


### C to Clojure

A large part of bare Emacs is pretty redundant in 2012, so this will be mapped to JVM languages, and exposed to Emacs Lisp as the same primitives it has come to know and love. A subset of the Emacs C code is dealing with buffers, regex and other editing specifics, which will be harder to just replace.

Bare impure Emacs is 203692 lines of C spread over 65 files and another 19912 lines of header files. There are around 1064 primitive `defsubr` in the minimal build. (These numbers are from Emacs 23.4.)

Clojure placeholders for some of the Emacs primitives live under [`deuce.emacs`](https://github.com/hraberg/deuce/tree/master/src/deuce/emacs). They are be generated by [`./build-stubs`](https://github.com/hraberg/deuce/blob/master/build-stubs) using [`deuce.scaffold`](https://github.com/hraberg/deuce/blob/master/test/deuce/scaffold.clj).


The actual porting of the C will be done using a tactic of avoidance until a function is needed, auto generation of its signatures second, and hand crafting the actual implementation last.


### The Editor

I don't expect the visual editor to exist for quite a while. Initially, the editor itself will be implemented using [Charva](http://www.pitman.co.za/projects/charva/index.html) (or similar) Java curses/console library to keep things as simple as possible, compatibility wise. Eventually Swing, SWT and browser based front ends can be added to the mix.


### Testing

Larger than the technical challenges - which are mainly about scale - is the fact it doesn't seem to be any large regression suite for Emacs one can use to ensure one is on the right track. There are some tests, and other editors, like Zile, have Emacs compatibility test suites for at least editing that could be reused:

* Emacs is using [`ert.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/ert.el) for regression testing. Stallman's [comments](http://lists.gnu.org/archive/html/emacs-devel/2007-12/msg01339.html).
* [Zile](http://www.gnu.org/software/zile/) [tests](http://git.savannah.gnu.org/cgit/zile.git/tree/tests) runs against both Zile and Emacs.
* [Org-mode](http://orgmode.org/worg/org-tests/index.html) [testing/README](http://repo.or.cz/w/org-mode.git/blob/HEAD:/testing/README)
* [Regression Testing XEmacs](http://www.xemacs.org/Documentation/21.5/html/internals_12.html) may or may not work with GNU Emacs.

[`./ert`](https://github.com/hraberg/deuce/blob/master/ert) will run [its own test suite](https://github.com/hraberg/deuce/blob/master/test/ert-tests.el), using the Emacs built at `emacs/src/emacs`.


### Building

`lein uberjar` will bundle together Deuce, Clojure and the Emacs Lisp from GNU Emacs into an executable jar (which currently can only do limited evaluation of Emacs Lisp in batch mode).


### The Road Map

My guess is that it will take roughly a month to get anything useful at all out of batch mode with basic Emacs Lisp cross compilation. An editor that can do anything but crashing another 2 months. An actual useful, somewhat compatible subset of Emacs 6 months.

A potential first milestone is to get [`ert.el`](https://github.com/ohler/ert) testing itself in batch mode.

Matching the performance and exact characteristics of the C code for buffers etc. isn't a goal.

100% compatibility is never expected, as the port needs to be driven by the need to support a useful, growing subset of Emacs Lisp packages and Emacs features.


### Future

Once Emacs works again, we can move it forward into the future, where it originally came from. I eventually envision something quite different from Emacs, but that may very well end up being another project all together. But having a useful subset of Emacs running on the JVM may come handy when one least excepts it.

The real goal is to bring back some of the fun of extending one's programming environment, by removing some of the old constraints and open up new possibilities - while respecting the Emacs tradition.


### Appendix: Which Approach?

This is the fun part. A mix of all these ideas and more may play a part.

The common theme here is that we have something that works: Emacs, and prefer to move as quickly as possible to something else that should be a small, but working, subset: Deuce. There are many ways to get lost on this road.

**The first foray out of the base camp will be a combination of porting the "ideal" Emacs Lisp runtime while clearing way for the "real" Emacs boot.**
I expect this to take 1-2 weeks and fail, but to learn a bit about how Emacs does things, and slowly adjust to the altitude.


* Start from the beginning. Get Emacs starting and take it from there.
  * + The most obvious approach. Easy to see where one is at.
  * + YAGNI can be used.
  * - Every step forward may derail into multiple sub problems, each one requiring it's own mindset and toolbox.
  * - False sense of security when you load all initial Emacs Lisp without evaluating any of it.

* Treat Emacs Lisp as it's own problem and solve it first.
  * + To some extent, this must be made, getting the basic semantics of Emacs Lisp ported on top of Clojure early on, as everything else hinges on it.
  * - Lacks clear delineation - what is a minimal Emacs Lisp runtime?
  * - Emacs Lisp is boring on its own.

* Roll up the sleeves and just port the damn thing, function by function until it starts working.
  * + It's simple to understand.
  * - It's hard to do. Risk of missing the woods for the trees.
  * - Impossible to know what to avoid, or verify that they're working as intended together.

* One approach is to embed a JVM inside Emacs, and let it eat its way out.
  * + Emacs stays working, A/B testing of individual functions can be made.
  * + One could maybe implement the Emacs Lisp runtime on top of Clojure this way, and slide it into a C bare impure Emacs, to divide the problem into two distinct parts.
  * - Requires writing messy and potentially buggy glue code in C, and may get stuck in the implementation details of bare impure Emacs.
  * - Hard to know how far one has to go.
  * - Two parallel Emacs Lisp runtimes to manage.

* Compile Emacs as an library, and actually call it from Java, and move more and more pieces over.
  * + A/B mode possible, you can run Emacs in this mode for testing, if nothing else.
  * - Still requires bootstrapping bare Emacs in Clojure, with the additional confusion of having to manage and share state with C.

* Event recording from working Emacs, playback in Deuce, alternatively multiplexing a user session, comparing the two Emacs Lisp runtimes live.
  * + Captures broad, real world, test cases.
  * - Only works later in the game, once Emacs Lisp is somewhat working.
  * - Requires infrastructure on the Emacs side, anything form C, Emacs Lisp meta programming to keyboard macros.

* Auto converting the C part to Clojure or Java, using either source to source similar to [Vacietis](https://github.com/vsedach/Vacietis) or LLVM IR, like [emscripten](emscripten.org).
  * + "Easy", assuming the converter exists, Emacs depends on very few libraries.
  * + Great if the code is readable.
  * - But it most likely won't be, and while turning C into readable Clojure is a fun problem, it's likely out of scope.
  * - Basing a port on generated source feels wrong and leads to a lack of hackability of the new core.

* Avoid porting functions at all costs.
  * + Self evident, less code is always better code. "This is simple!"
  * + Certain parts of Emacs are better backed by Java's encoding, regex and IO handling than it's own.
  * + Some functions will never be missed.
  * - Some attempts to side step old Emacs functions with impostors may back fire and lead down compatibility hell, and cutting corners may end up costly.
  * - Sometimes it's easier to just do it.

* Get to this editor, rumored to be bundled with Emacs, as quick as possible.
  * + This is actually what we want, isn't it?
  * + If the editor is visibly broken, one notices.
  * - Directly side tracks into screen buffer management and other potential time sinks.
  * - Risk of building the house without sound foundation.
  * - Seeing how broken it will be early on could be bad for morale.


### Scott McKay's "Dylan Environment Universal Code Editor"

I recently found out about this other Emacs clone, also named [Deuce](http://groups.google.com/group/comp.lang.lisp/msg/02782906f6c4d6e8?pli=1) (2001):

> Actually, I called it Deuce as a conscious homage to Zwei, then force-fit an acronym: Dylan Environment Universal Code Editor.


Scott then further talks about this:

> A buffer is then dynamically composed of "section nodes" [..] it costs a little in performance, but in return it's much easier to build some cool features [like fonts, graphics].

Which nicely leads into richer, HTML based versions - the general approach in "`deuce.clj`" is expected to aim for support of text based Emacs buffers, but skip the more modern (but obsolete) graphics features of GNU Emacs and head straight for the browser.

I'll revisit the name if the Clojure port actually becomes usable and the name clash with Dylan Deuce leads to confusion.


## References

[EMACS: The Extensible, Customizable Display Editor](http://www.gnu.org/software/emacs/emacs-paper.html) Richard Stallman, 1981

[GNU Emacs Lisp Reference Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)

[Emacs and Common Lisp](http://tromey.com/blog/?p=709) Tom Tromey, 2012

[Emacs Lisp in Edwin Scheme](http://www.dtic.mil/dtic/tr/fulltext/u2/a276721.pdf) Matthew Birkholz, 1993

[Thoughts On Common And Scheme Lisp Based Emacs](http://xahlee.org/emacs/modernization_of_elisp.html) Xah Lee, 2008

[The Craft of Text Editing: Emacs for the Modern World](http://www.finseth.com/craft/) Craig A. Finseth, 1991, 1999

[Down with Emacs Lisp: Dynamic Scope Analysis](http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/dynamic-scope-analysis.pdf) Matthias Neubauer and Michael Sperber, 2001

[Climacs - An Emacs-like editor in Common Lisp](http://common-lisp.net/project/climacs/)

[Portable Hemlock](http://www.common-lisp.net/project/phemlock/) Another Common Lisp Emacs.

[JEmacs - The Java/Scheme-based Emacs](http://per.bothner.com/papers/Freenix00/Freenix00.html) Per Bothner, 2000

[Zile](http://www.gnu.org/software/zile/) "Zile is lossy Emacs"

[uemacs](http://git.kernel.org/?p=editors/uemacs/uemacs.git;a=tree) Linus' micro Emacs.

[YMACS](http://www.ymacs.org/) Ymacs is an Emacs-like editor that works in your browser.

[Pymacs](https://github.com/pinard/Pymacs.git) Emacs to Python interface

[el4r](http://www.rubyist.net/~rubikitch/computer/el4r/index.en.html) EmacsRuby engine - EmacsLisp for Ruby


#### This is NOT YET GNU Emacs, one component of the GNU/Linux operating system.


## License

[GNU General Public License Version 3](http://www.gnu.org/licenses/)

[GNU Emacs](http://www.gnu.org/software/emacs/) is Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
  2010, 2011, 2012  [Free Software Foundation, Inc](http://www.fsf.org/).

# DEUCE - Deuce is (not yet) Emacs under Clojure

*Because it's there* -- George Mallory


**Note: Almost NOTHING works yet. 0.1.0 developer preview ETA: when it's done.**

Also - there's a risk I'll give up, far before reaching the current benchmark of JVM Emacsen: [JEmacs](http://per.bothner.com/papers/Freenix00/Freenix00.html).


### What is Deuce?

**Deuce is a re-implementation of Emacs in Clojure.** It's a port of the C core and re-compiles existing Emacs Lisp to Clojure. It uses the Lanterna library for text UI. The goal is to reach reasonable compatibility with GNU Emacs during 2014. The longer term goal is to phase out Emacs Lisp in favour for Clojure, to add a Web UI and re-capture Emacs' spirit on a contemporary platform.

[Talk](http://skillsmatter.com/podcast/scala/deuce-is-not-yet-emacs-under-clojure) at [Skills Matter](http://skillsmatter.com/) London on June 4 | [Slides](http://hraberg.github.io/deuce/skillsmatter/?full)

Deuce can currently start Emacs and handle limited keyboard input (like C-x C-c). The main moving pieces are there but it's far from usable yet. Back in 2013 I aimed to release a developer preview, 0.1.0 around September, followed by an end-user alpha "when it's done". Needless to say, this didn't happen. I also thought that 0.1.0 was less than a months work away, and that reaching alpha would take about another 2-4 months of work. While that still somewhat holds, I feel Deuce needs some new ideas to move forward.

I work on Deuce on and off and have spent about 2-3 months full-time so far in total. I started in April 2012.

To run:

    make run

### What is there to see?

[Marginalia](http://hraberg.github.io/deuce/) | [Skip to below updates](#preparing-emacs) | [Contributors](https://github.com/hraberg/deuce#contributors) | [Dependencies](https://github.com/hraberg/deuce#dependencies)

**2015-02-06**

I've revisited Deuce this past week and done some house cleaning to make it slightly more attractive to work on:

* Replaced the ad-hoc scripts with a single [`Makefile`](https://github.com/hraberg/deuce/blob/master/Makefile). The most interesting targets are `dist`, `test`, `run` and `run-dev`. The default target incrementally builds the uberjar and run the tests.
* Replaced the EDN based output of the Clojure generated from Emacs Lisp with a [Fipp](https://github.com/brandonbloom/fipp) based pretty-printer that mimics Emacs Lisp a bit to make the generated Clojure code easier to read and look more similar to the Emacs Lisp it comes from.
* Fixed a long standing issue where meta data - most importantly line numbers - got lost in the transformation from Emacs Lisp to Clojure, so now errors in the generated code contain the proper lines.
* Setup the ability to run [Zile's](http://www.gnu.org/software/zile/) [tests](http://git.savannah.gnu.org/cgit/zile.git/tree/tests?id=v2.4.11) [against Deuce](https://github.com/hraberg/deuce/blob/master/test/deuce/test/zile.clj) to guide the implementation of the basic editor primitives. One test, [`insert-char.el`](http://git.savannah.gnu.org/cgit/zile.git/tree/tests/insert-char.el?id=v2.4.11) is currently active and passing.
* Upgraded the dependencies to versions relevant for 2015, except for Lanterna which works and where the latest version runs into an issue.

The main issue right now for further development of Deuce is that it takes around 5-10 minutes to re-generate and AOT the Clojure from Emacs Lisp. This is only needed to do when one changes how Emacs Lisp is actually compiled, and not during work on the primitives implemented in Clojure - that is, the parts of Emacs written in C. But even simply running a fresh REPL and `(deuce.emacs.lread/load "deuce-loadup.el")` can take a few minutes, and it's easy to get into a state where one has to bounce the REPL. My immediate goal is to try to fix some of these issues, making it swifter and more fun to hack on Deuce. At the end of the day, that's always been the goal of this project - to have fun and learn Emacs lore.

The Emacs Lisp works pretty well these days (there are a few known issues), but there's a lot of it loaded when starting Emacs, and as far from all primitives are implemented or work properly, many things aren't in the right state after the Emacs boot. This leads to a situation where one fix one issue, only to unlock (Emacs Lisp - The Game!) an entire subsystem of Emacs after implementing a new set of primitives - which used to return `nil` - revealing further problems. That said, this isn't a bad thing, but rather showing the road forward - but it makes planning the effort one needs to put into a single coding session quite hard, which makes it easy to give up.

The terminal UI and keyboard handling (there's been some regression here) need plenty of work, I spent some time trying to replace this entire thing with a web UI a year back, somewhat related to `dg` below - which was written in JavaScript, but backed off from that effort for various reasons while learning some interesting things. That said, the longer time plan of Deuce has never been to have a text UI embedded in the actual editor, which is meant to be a server for various front-ends.


**2013-11-28 At work**

I'm currently working on another Clojure project (paid, "real" work), which will last into next year, hence the inactivity here. The good news is that it should support time for open source hacking afterwards.


**2013-07-08 Deuce will be back**

I gave [a talk about Deuce](http://skillsmatter.com/podcast/scala/deuce-is-not-yet-emacs-under-clojure) last month here in London to explain to people, including myself, what this project is about. My current plan is to dedicate August [2013-08-26: Looks like this didn't pan out, so let's just say sometime this year :-)] to get it to a releasable state. The acceptance criteria is still to be able to run the Emacs tutorial (see 2013-03-22 below).

I'm currently exploring the entire problem from a different angle. This project is called `dg`, which is browser-first, written in JavaScript. Emacs-like, no clone, no legacy, no features:

<img width="683" src="https://github.com/hraberg/deuce/raw/master/screenshots/dg-ido-mode-with-codemirror.png" alt="D&G isn't Deuce" title="D&G isn't Deuce" />

*Ceci n'est pas une Emacs.*


**2013-04-08 C-x C-c**

After a break I'm now revisiting the command loop. You can start Deuce with `lein trampoline run -Q`  to get into `*scratch*` and type away, most things won't work - you can tail `~/deuce.d/deuce.log` to see what's happening (or not).

Keymaps more or less work, as does using [`term/xterm`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/term/xterm.el) to setup `input-decode-map` which translates escape codes into Emacs events, like this:

    (define-key map "\e[1;3C" [M-right]) ;; This is in turn mapped to right-word somewhere else.

This requires us to bypass Lanterna's own input system, so we're only using Lanterna to get into private mode and draw things now.
`C-z` will suspend Deuce, `fg` brings it back. Doing `C-x C-c` will exit Deuce (there are still reasons to do this).


**2013-03-23 Lanterna**

Lanterna is now default, it renders the UI in busy loop until we got something better, you start it like this:

    lein trampoline run -q --swank-clojure

You can then connect via Swank (or nREPL with `--nrepl`) and issue a few commands and see Deuce reflect them in the terminal:

```clj
user> (in-ns 'deuce.emacs)
;; We're now in Emacs Lisp

(switch-to-buffer "*Messages*") ;; Shows the boot messages, Loading ...etc.
(switch-to-buffer "*scratch*")
(insert "Deuce is (not yet) Emacs under Clojure")
(beginning-of-line) ;; Ctrl-a
(kill-line) ;; Ctrl-k
(yank) ;; Ctrl-y
```

Here's an example how it looks, I have a buffer appender for the [`timbre`](https://github.com/ptaoussanis/timbre) debug log, you can also see the mark/region being active:

<img width="576" src=https://github.com/hraberg/deuce/raw/master/screenshots/deuce-debug-buffer.png alt="Deuce is (not yet) Emacs under Clojure - *Deuce*" title="Deuce is (not yet) Emacs under Clojure - *Deuce*" />

I'm currently working on the keymaps, so we can start accepting real keyboard input in a simple command loop.


**2013-03-22 What needs to be done for 0.1.0?**

0.1.0 is in my mind a version of Deuce running in a terminal which can open `*scratch*`, visit files and do normal text editing in a few buffers, and allows executing some simple commands via the minibuffer and complete the Emacs learn-by-doing tutorial. Nothing more than this.

This is roughly the order the remaining bits has to be solved in:

* [Command Loop](http://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Loop.html)
  * Emacs command loop is pretty complicated, so I won't aim to support recursive edits etc, but we want the basic feel of it.
  * This obviously implies [Keyboard Events](http://www.gnu.org/software/emacs/manual/html_node/elisp/Keyboard-Events.html).
*  What to do with this is determined  by the ["Active Keymaps"](http://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Keymaps.html). [Keymaps](http://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html) are almost there in Deuce, but needs overhaul now when actually about to be used.
  * There are ways of suppressing keymaps, have keymaps for just part of a buffer etc. and not everything will work.
* [Minibuffers](http://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers.html)
  * You can't claim its Emacs without the minibuffer. Again, there are many subtle ways Emacs can throw a minibuffer at you, and won't deal with them all at first.
  * Once we have the minibuffer, we need to support [Interactive Calls](http://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Call.html) at least to some extent. See [Using `interactive`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html) for more.
  * The minibuffer obviously also has [Completion](http://www.gnu.org/software/emacs/manual/html_node/elisp/Completion.html). We have some of the rudimentary backend support for this, but will have to be refined once we reach this point.
* [Windows](http://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html) - we have parts of this, and will ignore some others, but we need to support [splitting](http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html) to at least to at one level, as many Emacs commands will throw an extra window at you. We also want at least part of the logic for selecting windows and switching buffers (the primitives are there, but lacks all the fancy logic to figure out what to switch to).
* Tutorial / [Basic Editing Commands](http://www.gnu.org/software/emacs/manual/html_node/emacs/Basic.html) I want it to be possible to complete the Emacs Tutorial (the non-frame parts), this is basically the acceptance criteria for 0.1.0.

Notable things that won't be supported in 0.1.0:

* [Lisp Interaction Mode](http://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html).
  * Deuce can obviously evaluate (most) Emacs Lisp, but to make this actual mode really work, you need to support the [Syntax](http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Basics.html) subsystem.
* [Loading](http://www.gnu.org/software/emacs/manual/html_node/elisp/How-Programs-Do-Loading.html) of arbrirarty Emacs Lisp outside of `deuce-loadup.el`
  * Doing so may or may not work, but very likely this will attempt to call some native (C)lojure part of Deuce that doesn't exist or won't work for this particular case.
  * While [Autoload](http://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html) works, whatever that gets loaded probably won't
* [Frames](http://www.gnu.org/software/emacs/manual/html_node/emacs/Frames.html) Deuce only supports one single frame.
* [Text Properties](http://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html) and [Overlays](http://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html)
  * Text properties are double trouble, as the main reason to implement them is to vastly complicate your rendering to support bold, italic etc. They can also be copied between buffers and other exciting things.
  * If time allows, I might add rudimentary support for overlays, they're implemented on top of markers, and would enable links.
* When running out of the standalone jar, Deuce still compiles newly loaded files to `target/classes`, which isn't on the classpath.
* Speed
  * Deuce takes about a minute to start, which I've decided is "good enough" for 0.1.0.
  * Deuce won't be your platform of choice for any high-performance Emacs Lisp needs anytime soon.
* All the exciting bits when Clojure actually takes the front seat. Web UI.
  * First Deuce needs to work for Emacs Lisp in "legacy mode" before we can shift focus to Clojure and modernize things. I expect this to stay as the focus for the rest of 2013.
* Anything else you can think of!


It's hard to estimate how long this will take, sometimes you accidentally implement a subsystem of Emacs in a handful of lines of Clojure, but more often you get stuck in subtle details (writing almost no code) reading the Emacs C source desperately trying to figure out how the heck something is supposed to be working. Often you also get into a state where you're married to your 80% solution for a problem, but in your heart of heart know you'll have to ditch and rewrite it to do the "proper" thing.

I usually switch between two approaches: trying to do what Emacs does based on treating it as a black box and implementing a function based on its documentation, or actually understand the source of what it -really- does. The best approach so far seems to be the former, and only occasionally take a peak at the source to disambiguate corner cases. But the closer to the core you get, the more this model breaks down. Something like the command loop for example is really a C part of Emacs, it's not a user level API - it is the main loop of the program itself. Implemented in screen after screen of wonderful C honed over 30 years.

Anyway, I will continue to work full time on Deuce until April 25, so hopefully - 0.1.0 before then!


**2013-03-19 Mode Lines and Menu Bar**

We're getting there, now with [Lanterna](https://code.google.com/p/lanterna/):

<img width="648" src=https://github.com/hraberg/deuce/raw/master/screenshots/deuce-lanterna.png alt="Deuce is (not yet) Emacs under Clojure - *scratch*" title="Deuce is (not yet) Emacs under Clojure - *scratch*" />

Doesn't take windows sizes, scrolling or splits into account, as none of these are even implemented, but you can now conveniently edit text from the REPL and echo your edits with `deuce.main/display-using-lanterna`. The very opposite of a display editor! The echo area message should also been cleared, but the command loop that deals with that doesn't exist yet either, so the edits are all made via lower level APIs, like `insert`, `goto-char` etc.


**2013-03-17 Visiting Files**

One of those features that just make Emacs so great as an editor is its ability to load files:

```
lein run -q ~/project.clj
Loading deuce-loadup.el (source)...
[ ... ]
--------------- #<buffer project.clj> --- [current buffer]
(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is (not yet) Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"

  ...)
```

I first tried loading this from my local checkout, which ended up trying to autoload `vc-git` and various dependencies - `desktop.el` which blows up in the Clojure compiler with a `ClassFormatError: Illegal field name "q.txt"`:

```el
(let ((q.txt (desktop-internal-v2s (car p))))
    ...)
```
A real gem.

There's also heuristics in `auto-mode` that tries to figure out what kind of file was just loaded to enable the right modes, most which aren't supported yet, as it tries to search around and do various analysis on the file contents.

One "drawback" right now during the early days, is that when you try to incrementally add stuff, you can never be sure what Emacs is going to throw at you - with `autoloads` and `hooks` all bets are basically off.


**2013-03-16 Basic Buffers**

Buffers are naturally pretty complex beasts in Emacs. But we got the basics: switching and inserting text in different buffers. Overlays, buffer locals, markers etc. are not there yet:

    # Start with Swank so you can poke around:
    lein run -q --swank-clojure
    Swank connection opened on 4005
    Loading deuce-loadup.el (source)...
    Using load-path ("" "emacs-lisp" "language" "international" "textmodes")
    Loading emacs-lisp/byte-run...

    [... eventually calling deuce.main/display-buffers to dump the state to stdout ...]

    --------------- #<buffer *GNU Emacs*> --- [current buffer]
    Welcome to GNU Emacs, a part of the GNU operating system.

    Get help	   M-x help
    Emacs manual	   \[info-emacs-manual]	Browse manuals	   \[info]
    Emacs tutorial	   \[help-with-tutorial]	Undo changes	   \[undo]
    Buy manuals	   \[view-order-manuals]	Exit Emacs	   \[save-buffers-kill-terminal]
    Activate menubar   \[tmm-menubar]
    (`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
    If you have no Meta key, you may instead type ESC followed by the character.)
    Useful tasks:
    Visit New File			Open Home Directory
    Customize Startup		Open *scratch* buffer

    GNU Emacs 24.2 (jvm-1.8.0-ea_clojure-1.5.1)
     of 2013-03-16 on X202E
    Copyright (C) 2012 Free Software Foundation, Inc.

    GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \[describe-no-warranty] for full details.
    Emacs is Free Software--Free as in Freedom--so you can redistribute copies
    of Emacs and modify it; type \[describe-copying] to see the conditions.
    Type \[describe-distribution] for information on getting the latest version.
    --------------- #<buffer  *Echo Area 1*>

    --------------- #<buffer  *Echo Area 0*>
    For information about GNU Emacs and the GNU system, type \[about-emacs].
    --------------- #<buffer  *Minibuf-0*>

    --------------- #<buffer *Messages*> --- [see stdout above]
    --------------- #<buffer *scratch*>
    ;; This buffer is for notes you don't want to save, and for Lisp evaluation.
    ;; If you want to create a file, visit that file with C-x C-f,
    ;; then enter the text in that file's own buffer.

Now you can connect via Slime to the Swank server running inside Deuce and evaluate Emacs Lisp:

```clojure
;; deuce.emacs is the "Emacs Lisp" namespace, clojure.core is required as c.
(in-ns 'deuce.emacs)

(defun factorial (integer)
  "Compute factorial of an integer."
  (if (= 1 integer) 1
    (* integer (factorial (#el/sym "1-" integer))))) ; Note the reader macro for 1-
;=> factorial
(factorial 10)
;=> 3628800

(current-buffer)
;=> #<buffer *GNU Emacs*>

(emacs-version)
;=> "GNU Emacs 24.2 (jvm-1.8.0-ea_clojure-1.5.1)\n of 2013-03-16 on X202E"
```

To kick off the terminal:

```clojure
;; Switch back to the safety and sanity of Clojure land:
(c/in-ns 'deuce.main)

(require '[lanterna.screen :as s])
(def screen (s/get-screen :text)) ;; or :swing
(s/start screen) ;; Clears the terminal Deuce is running in.

;; Try to draw something useful or react to the keyboard - both still to be done.
;; See deuce.test.terminal for the general idea.

(s/stop screen) ;; Restores the terminal to what it was. Ctrl-C kills Swank.
```

To display something for real, we need Emacs windows, of which the minibuffer is one, and finally at least one Emacs frame - a real "OS" window or terminal. To this mix you add the keymaps, which are basically nested in trees, looking somewhat like this:

```el
slime-mode-map
(keymap
  (24 keymap  ;; Sub keymap for C-x
    (5 . slime-eval-last-expression) ...) ...) ;; Containing C-e, and we now know what to call.
````

Emacs has many exciting ways of describing keys though (the Deuce parser cannot handle all variants), and there are always more than one keymap in play at once, see ["Active Keymaps"](http://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Keymaps.html) in the unlikely event you're interested in this. As a bonus, the menu bar is a also a keymap.


**2013-03-14 [`deuce-loadup.el`](https://github.com/hraberg/deuce/blob/master/src/deuce-loadup.el)**

The final stretch took time, with each issue taking days to analyze and debug, inevitably leading to subtle - but crucial - changes to [`deuce.emacs-lisp`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp.clj) (`unquote-splicing` working for atoms at the end of a list? Why not!).

Deuce currently consist of around 12.5k LOC, a majority of which are doc strings and empty stubs generated from Emacs. The files loaded by `loadup.el` compiles into a total of 100k LOC "readable" Clojure. "Dumping" Deuce to an uberjar takes about 10 minutes (excluding the initial Emacs steps):

    # Emacs 24.2 is assumed to live under ./emacs, for details, see Preparing Emacs below.
    # loaddefs.el and a few other files are generated by the Emacs build so you need to run:
    ./configure-emacs # Will clone emacs the first time (~1Gb).
    make -C emacs

    # Start Deuce, all Emacs Lisp referenced by `deuce-loadup.el` is turned into Clojure and compiled to `.class` files:
    lein run -q
    Loading deuce-loadup.el (source)...
    Using load-path ("" "emacs-lisp" "language" "international" "textmodes")
    Loading emacs-lisp/byte-run...
    [... lots of Emacs Lisp loaded and compiled into .class files under target/classes, takes about 5 minutes ...]
    Welcome to GNU Emacs, a part of the GNU operating system.
    [... the welcome screen is printed to stdout ...]
    Type \[describe-distribution] for information on getting the latest version.

    # Sanity check, done afterwards as this would otherwise compile a few things:
    lein difftest

    # AOT the Clojure under `deuce.emacs` and uberjar everything, takes a few minutes, the resulting jar is about 65Mb:
    lein uberjar

    # Start Deuce from the jar (it will exit as there's no editor yet), takes about a minute:
    ./deuce # java -jar target/deuce-0.1.0-standalone.jar

    # You can execute Emacs Lisp:
    ./deuce --batch --eval "(print (emacs-version))"


Focus now changes to the Deuce internals: buffers, windows, keymaps and most crucially - a display for this extensible, customizable editor! The core Emacs Lisp runtime is still more complicated than it has to be, and there are some known issues - like proper closures and `setq` + `delayed-eval` (used for things that cannot be resolved at compiled time) not working during binding in `let`. Speed is another issue.


**2013-03-08 Mutable Persistent Lists**

Exactly the what it sounds like - `setcar` and `setcdr` working directly on [`clojure.lang.PersistentList`](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentList.java). This is an experimental branch which I will keep investigating this for a day or two. Most of the relevant stuff lives in [`deuce.emacs-lisp.cons`](https://github.com/hraberg/deuce/blob/MutablePersistentLists/src/deuce/emacs_lisp/cons.clj)

There are a few reasons why I investigate this:

* Data created via reader macros, like `#deuce/cons`, cannot (easily) participate in syntax quoting.
* Deuce has many checks and conversions of what kind of list is getting passed around (won't be fully solved by this).
* Dotted pairs have the literal representation: `'(1 . 2)` , and is handled internally via protocols. This also allows them to participate properly in syntax quoting. A non-list `cdr` is hence a two element list: `(. 2)`.
  * This has the serious drawback that a pair can "escape" to a Clojure function not understanding the dot.

There are several other things on this branch that will be merged back regardless, using [`ex-info`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/ex-info) for Emacs Lisp errors and new tests for core functions - and hence more correct implementations. Also: `defalias` and `autoload` that actually works for macros.

Before embarking on this, I was close to have most of the relevant Emacs Lisp required to actually start up Emacs (see below) loaded, but the final mile requires `cl.el` to really shine and let its macros expand like intended. Its currently stuck on [`pcase.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/emacs-lisp/pcase.el) ("ML-style pattern-matching macro for Elisp"), which is needed for [`minibuffer.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/minibuffer.el) and others. The state of this branch is currently stuck much earlier, but I'm working on it to see if I can get `pcase` (and the full boot) to actually work via this approach.


**2013-03-03 Welcome to GNU Emacs**

No terminal or proper frame, just using standard out as buffer:

    $ lein run -q
    Loading deuce-loadup.el...
    Using load-path ( emacs-lisp language international textmodes)
    Loading emacs-lisp/byte-run...
    Loading emacs-lisp/backquote...
    Loading subr...
    Loading version.el...
    Loading widget...
    Loading custom...
    Loading emacs-lisp/map-ynp...
    Loading cus-start...
    Loading international/mule...
    Loading international/mule-conf...
    Loading env...
    Loading format...
    Loading bindings...
    Loading window...
    Loading files...
    Loading cus-face...
    Loading faces...
    Loading button...
    Loading startup...
    Loading loaddefs.el...
    Loading term/tty-colors...
    Loading paths.el...
    (src/bootstrap-emacs)
    For information about GNU Emacs and the GNU system, type \[about-emacs].
    Welcome to GNU Emacs, a part of the GNU operating system.

    Get help	   M-x help
    Emacs manual	   \[info-emacs-manual]	Browse manuals	   \[info]
    Emacs tutorial	   \[help-with-tutorial]	Undo changes	   \[undo]
    Buy manuals	   \[view-order-manuals]	Exit Emacs	   \[save-buffers-kill-terminal]
    Activate menubar   \[tmm-menubar]
    (`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
    If you have no Meta key, you may instead type ESC followed by the character.)
    Useful tasks:
    Visit New File			Open Home Directory
    Customize Startup		Open *scratch* buffer

    GNU Emacs 24.2 (jvm-1.7.0_13_clojure-1.5.0)
     of 2013-03-03 on X202E
    Copyright (C) 2012 Free Software Foundation, Inc.

    GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \[describe-no-warranty] for full details.
    Emacs is Free Software--Free as in Freedom--so you can redistribute copies
    of Emacs and modify it; type \[describe-copying] to see the conditions.
    Type \[describe-distribution] for information on getting the latest version.

It is nice to get a feel for what Emacs actually tries to do when it starts, as the sea of Emacs Lisp now becomes a bit more managable to navigate when you can see the other shore.

Several important files aren't loaded yet, I had to provide stubs for about 10 functions, primiarly related to initializing the frame, to make `normal-top-level` actually display the welcome screen. Also, buffer management doesn't really exist yet except as stubs, it just prints to the standard out.

The generated Clojure now contains several Clojure 1.5 reader macros to represent Emacs Lisp. The Cons lists are back to Java, as there seems to be a bug in the Clojure compiler when compiling deftypes with mutable fields.


**2013-03-02**

[`deuce-loadup.el`](https://github.com/hraberg/deuce/blob/master/src/deuce-loadup.el) now contains comments and road map towards [0.1.0](https://github.com/hraberg/deuce/issues?milestone=1). I aim to get out of the low-level Emacs Lisp fight during the next week and start focus on the rough implementations of the actual Emacs primitives like keymaps, buffers, windows and the terminal and tie it all together to an actual editor.


**2013-02-28**

The new evaluation model is in. This creates a `.clj` file for each `.el` file it loads, which can then be compiled into a `.class` file. The use of Clojure's `eval` is gone, and the dynamic scoping is now all done by vars (which may be accessed lexically). The core is also vastly simplified, it had many defensive post-walks for opaque root causes which the slow load time made very hard (for the impatient) to debug.

While the initial compilation actually is slower, it opens up for a better development model overall, as you can actually see and reevaluate the generated Clojure while solving an issue (and get proper line numbers), and once compiled, it loads much faster - `lein uberjar` can now basically be used to create an Emacs which isn't "bare".

Also new is [Bjarte](https://github.com/ljos)'s work on proper Cons lists, which allows for proper (but insane) semantics of destructive updates on mutually shared cells. `deuce-loadup` has slipped back a bit, but now I can refocus on the boot. As mentioned below, I will likely move to a more hand coded version of `loadup` which skips cumbersome parts of dubious immediate value to the full Emacs boot.

    $ lein run --batch --eval "(print (emacs-version))"

    Loading deuce-loadup.el...
    Using load-path ("" "emacs-lisp" "language" "international" "textmodes")
    Loading emacs-lisp/byte-run...
    Loading emacs-lisp/backquote...
    Loading subr...
    Loading version.el...
    GNU Emacs 24.2 (jvm-1.7.0_13_clojure-1.5.0-RC17)
     of 2013-02-28 on X202E


I've managed to establish a hacking base in Agonda now. No sleep until `M-x butterfly`.


**2013-02-25**

After a long break from Deuce, I'm now in Agonda, Goa, specifically with the aim to hack on Deuce for two months. Thanks to [Bjarte](https://github.com/ljos) and [Navgeet](https://github.com/Navgeet) for their pull requests, despite my lack of focus here.

I plan to start with a rethink of how the Emacs Lisp evaluation model works - it's currently way too slow, and relies heavily on Clojure's `eval`. I'm also considering to just ditch or stub out some parts of `loadup` (specifically `mule`) for now and aim to get the editor actually starting in some form.


**2012-11-08**

After many false starts, [`cl.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/emacs-lisp/cl.el) now loads. I don't expect much of it to actually work, but some things do. See the [EmacsWiki](http://www.emacswiki.org/emacs/CommonLispForEmacs) [CommonLispForEmacs](http://www.emacswiki.org/emacs/CommonLispForEmacs) page for a discussion about when this file is actually supposed to be loaded.

> Packages distributed within GNU Emacs don’t use cl at runtime

Instead files like [`env.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/env.el) use

```el
(eval-when-compile (require 'cl))
```

The documentation to `eval-when-compile` says

> In interpreted code, this is entirely equivalent to `progn'

Compiling Emacs Lisp to Clojure counts as interpreting in Deuce - but anything in [`loadup.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/loadup.el) is normally already compiled when starting Emacs. When running Emacs with `-Q` you can see that `(featurep 'cl)` actually is `nil`, so in theory `cl.el` shouldn't be loaded, but this distinction is messy as things can depend on the `cl.el` macros. For obvious reasons it would be nice to avoid it, as we have a situation where Deuce contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp in an ad hoc, informally-specified, bug-ridden, slow implementation of half of Emacs Lisp in Clojure.

Next up is [`bindings.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/bindings.el) which is its own area of strangeness, but at least something related to the actual editor.


**2012-10-09**

Deuce is not dead. I've been to the US past few weeks, reflecting on StrangeLoop - hacking on a few new things.
I'm now back in London, gearing up to tackle the method length issue, see below.

**2012-09-21**

I've been working on [Shen.java](https://github.com/hraberg/Shen.java) last week, so no progress here.
Currently [`cl-macs.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/emacs-lisp/cl-macs.el) fails with `ClassFormatError: Invalid method Code length` in `cl-parse-loop-clause`.

**2012-09-11 Logging**

Using [Timbre, a (sane) logging library for Clojure](https://github.com/ptaoussanis/timbre) (Taoussanis, 2012).

**2012-09-10 Mutable Data**

Somewhat reluctantly now backing Emacs Lisp with [LinkedList](http://docs.oracle.com/javase/7/docs/api/java/util/LinkedList.html) for [lists](http://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html) and arrays for [vectors](http://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html), so various destructive updates work. Both [`custom.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/custom.el) and [`mule.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/international/mule.el) are using this to build up various state. I've also decided to represent property lists as meta data on the var for now.

Here's the current state - [`load-path`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Loading.html) entries are classpath relative (from [`emacs/lisp`](https://github.com/mirrors/emacs/tree/master/lisp), see [`project.clj`](https://github.com/hraberg/deuce/blob/master/project.clj)):


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

* Getting [`subr.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/subr.el) to load properly (and then continue [`loadup.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/loadup.el)).
* Proper loading of Emacs Lisp in general.
* Proper handling of lexical vs. dynamic scoping.
* Getting the [`ert.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/emacs-lisp/ert.el) [`ert-tests.el`](https://github.com/hraberg/deuce/blob/master/test/ert-tests.el) self tests running. This will require some buffer variable handling. [ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html) also requires a few other files to work (like [`cl.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/emacs-lisp/cl.el)).

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

[`deuce-loadup.el`](https://github.com/hraberg/deuce/blob/master/src/deuce-loadup.el) represents a small fraction of the real Emacs [`loadup.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/loadup.el). Not all forms loaded are even properly evaluated yet, but we get to [`version.el`](https://github.com/emacsmirror/emacs/blob/emacs-24/lisp/version.el).

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

A test representing a tiny fraction of [`loadup.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/loadup.el), [`loadup.clj`](https://github.com/hraberg/deuce/blob/master/test/deuce/test/loadup.clj).


**2012-04-22 The Emacs Lisp parser**

    lein run -m deuce.test.parser

It can read all the `.el` files under `emacs/lisp`, but the actual representation as Clojure forms will probably change.

[`parser.cjl`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp/parser.clj). This was one of the first things written back right before [EuroClojure](http://euroclojure.com/2012/), before I put Deuce on hold after admitting that the scope of this project required full time dedication.


### Preparing Emacs

The target version of Emacs is 24.2. It lives under `emacs` as a git submodule.

For a minimal [Emacs build](http://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.html):

    make temacs # Will clone emacs the first time (177Mb).
                # Might complain about missing `libtool`.
                # takes a while, only needs to be run once.

    ./emacs/src/temacs -Q --batch --eval "(print (emacs-version))" # make emacs-smoke

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

Run `make TAGS` and add something like this to your `init.el`:

```el
;; To navigate between C (or Clojure) and Emacs Lisp
(require 'etags-select)
(require 'etags-table)

(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key "\M-," 'pop-tag-mark)
(setq etags-table-search-up-depth 10)
(setq etags-select-go-if-unambiguous t)
```

There are probably better and cleaner ways of doing this, as TAGS includes TAGS-LISP (there's a hint at [here](http://www.emacswiki.org/emacs/EtagsSelect)).


### Hacking

While the most of the document below represents speculation about how the port might work, this discusses how the port *actually* works.

You need Emacs (see above) to have access to the Emacs Lisp, but actually building Emacs is optional (this is the entire point of Deuce).
I use Emacs to develop Deuce, which has a few recursive advantages - you can constantly verify how things should work in a real Lisp Interaction buffer.

After a few days of hacking, I found that it's easiest to load and switch to the [`deuce.emacs`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs.clj) and use this as your "base" namespace (`clojure.core` is required as `c`). Most actual hard work goes into [`deuce.emacs-lisp`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs_lisp.clj). The various namespaces under [`deuce.emacs`](https://github.com/hraberg/deuce/blob/master/src/deuce/emacs) replaces the [C core](https://github.com/mirrors/emacs/tree/master/src) of GNU Emacs.

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

Clojure placeholders for some of the Emacs primitives live under [`deuce.emacs`](https://github.com/hraberg/deuce/tree/master/src/deuce/emacs). They are be generated by `make stubs` using [`deuce.scaffold`](https://github.com/hraberg/deuce/blob/master/test/deuce/scaffold.clj).


The actual porting of the C will be done using a tactic of avoidance until a function is needed, auto generation of its signatures second, and hand crafting the actual implementation last.


### The Editor

I don't expect the visual editor to exist for quite a while. Initially, the editor itself will be implemented using [Charva](http://www.pitman.co.za/projects/charva/index.html) (or similar) Java curses/console library to keep things as simple as possible, compatibility wise. Eventually Swing, SWT and browser based front ends can be added to the mix.


### Testing

Larger than the technical challenges - which are mainly about scale - is the fact it doesn't seem to be any large regression suite for Emacs one can use to ensure one is on the right track. There are some tests, and other editors, like Zile, have Emacs compatibility test suites for at least editing that could be reused:

* Emacs is using [`ert.el`](https://github.com/mirrors/emacs/blob/emacs-24/lisp/emacs-lisp/ert.el) for regression testing. Stallman's [comments](http://lists.gnu.org/archive/html/emacs-devel/2007-12/msg01339.html).
* [Zile](http://www.gnu.org/software/zile/) [tests](http://git.savannah.gnu.org/cgit/zile.git/tree/tests) runs against both Zile and Emacs.
* [Org-mode](http://orgmode.org/worg/org-tests/index.html) [testing/README](http://repo.or.cz/w/org-mode.git/blob/HEAD:/testing/README)
* [Regression Testing XEmacs](http://www.xemacs.org/Documentation/21.5/html/internals_12.html) may or may not work with GNU Emacs.


### Building

`lein uberjar` will bundle together Deuce, Clojure and the Emacs Lisp from GNU Emacs into an executable jar. See the **2013-03-14 `deuce-loadup.el`** update above for details.


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

### Contributors

[Navgeet Agrawal](https://github.com/Navgeet), [Ragnar Dahlén](https://github.com/ragnard), [Kevin Downey](https://github.com/hiredman), [Bjarte Johansen](https://github.com/ljos), [Ryan Kelker](https://github.com/runexec), [Håkan Råberg](https://github.com/hraberg) and [Martin Trojer](https://github.com/martintrojer).

### Dependencies

For details, see [`project.clj`](https://github.com/hraberg/deuce/blob/master/project.clj):


[GNU Emacs](http://www.gnu.org/software/emacs/) Richard Stallman / FSF, 1985 - 2013 - "GNU Emacs is an extensible, customizable text editor—and more." GPL

[Clojure](http://clojure.org/) Rich Hickey, 2008-2013 - "Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy and a powerful macro system." EPL

[clojure-lanterna](https://github.com/sjl/clojure-lanterna) Steve Losh, 2012 - "A Clojurey wrapper around the [Lanterna](https://code.google.com/p/lanterna/) terminal output library." LGPL

[Lanterna](http://code.google.com/p/lanterna/) Martin Berglund, 2008-2013 - "Lanterna is a Java library allowing you to write easy semi-graphical user interfaces in a text-only environment." LGPL

[Timbre](https://github.com/ptaoussanis/timbre) Peter Taoussanis, 2012 - "a (sane) logging library for Clojure" EPL

[dynapath](https://github.com/tobias/dynapath) Tobias Crawley, 2012 - "A Clojure abstraction for modifiable/readable class loaders." EPL

[Fast Idiomatic Pretty-Printer](https://github.com/brandonbloom/fipp) Brandon Bloom, 2013 - "Fipp is a better pretty printer for Clojure." EPL


## References

[EMACS: The Extensible, Customizable Display Editor](http://www.gnu.org/software/emacs/emacs-paper.html) Richard Stallman, 1981

[GNU Emacs Lisp Reference Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)

[Emacs and Common Lisp](http://tromey.com/blog/?p=709) Tom Tromey, 2012

[Emacs Lisp in Edwin Scheme](http://www.dtic.mil/dtic/tr/fulltext/u2/a276721.pdf) Matthew Birkholz, 1993

[Thoughts On Common And Scheme Lisp Based Emacs](http://xahlee.org/emacs/modernization_of_elisp.html) Xah Lee, 2008

[The Craft of Text Editing: Emacs for the Modern World](http://www.finseth.com/craft/) Craig A. Finseth, 1991, 1999

[Down with Emacs Lisp: Dynamic Scope Analysis](http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/dynamic-scope-analysis.pdf) Matthias Neubauer and Michael Sperber, 2001

[Neovim](http://neovim.org/) Thiago de Arruda et al, 2014-2015 "Neovim is a project that seeks to aggressively refactor Vim."

[Internal Reprogrammability](http://martinfowler.com/bliki/InternalReprogrammability.html) Martin Fowler, 2013

[The Text Editor sam](http://plan9.bell-labs.com/sys/doc/sam/sam.html) Rob Pike, 1987

[Tour de Babel](https://sites.google.com/site/steveyegge2/tour-de-babel) Steve Yegge, 2004 - "Emacs is the 100-year editor."

[The Emacs Problem](https://sites.google.com/site/steveyegge2/the-emacs-problem) Steve Yegge, 2005

[In the Beginning was the Command Line](www.cryptonomicon.com/beginning.html) Neal Stephenson, 1999 - "Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It is not just bigger and brighter; it simply makes everything else vanish."

[Climacs - An Emacs-like editor in Common Lisp](http://common-lisp.net/project/climacs/)

[Portable Hemlock](http://www.common-lisp.net/project/phemlock/) Another Common Lisp Emacs.

[JEmacs - The Java/Scheme-based Emacs](http://per.bothner.com/papers/Freenix00/Freenix00.html) Per Bothner, 2000

[Lisp Computing Environment (LiCE)](http://texteditors.org/cgi-bin/wiki.pl?LiCE) Shawn Betts, 2005-2007 "LiCE is a GNU Emacs clone written entirely in common lisp." [Shawn](github.com/sabetts) is the creator of [stumpvm](https://github.com/sabetts/stumpwm) [GitHub mirror](https://github.com/spacebat/lice)

[Conkeror](http://conkeror.org/) Shawn Betts, 2004-2011 "Conkeror is a keyboard-oriented, highly-customizable, highly-extensible web browser."

[Zile](http://www.gnu.org/software/zile/) "Zile is lossy Emacs"

[uemacs](http://git.kernel.org/?p=editors/uemacs/uemacs.git;a=tree) Linus' micro Emacs.

[YMACS](http://www.ymacs.org/) Ymacs is an Emacs-like editor that works in your browser.

[Pymacs](https://github.com/pinard/Pymacs.git) Emacs to Python interface

[el4r](http://www.rubyist.net/~rubikitch/computer/el4r/index.en.html) EmacsRuby engine - EmacsLisp for Ruby


#### This is NOT YET GNU Emacs, one component of the GNU/Linux operating system.


## License

**Note: There are some issues distributing EPL and GPL code together, will investigate the implications when and if we get to a real release.**

* [EPL/GPL Commentary](http://mmilinkov.wordpress.com/2010/04/06/epl-gpl-commentary/) from the Eclipse Foundation.
* [Using the GPL for Eclipse Plug-Ins](http://www.fsf.org/blogs/licensing/using-the-gpl-for-eclipse-plug-ins) from FSF.

[GNU General Public License Version 3](http://www.gnu.org/licenses/gpl-3.0.html)

[GNU Emacs](http://www.gnu.org/software/emacs/) is Copyright (C) 1993-1999, 2001-2013 [Free Software Foundation, Inc](http://www.fsf.org/).

# Deuce Web UI Spikes

This directory contains a bunch of spikes, all written in JavaScript.

The main spike is `components`. To run:

```
    npm install
    node --harmony components-server.js
```

Then open `components.html` in a recent Chrome / WebKit.

This is a small editor written in JavaScript, where all logic lives on the server.
The server uses `rope.js` which is a persistent tree backing the buffers.

The client sends key events down to the server and receives patches for its view state back. It uses these to update its virtual dom.
It uses web components / shadow dom to create editor specific markup.


### Plan

I plan to finish this as a standalone editor, independent of Deuce with functionality roughly comparable to Zile. Once that is done I plan to explore a few avenues:

1. Integrating Clojure into this light JavaScript Zile-like editor.
2. Consider to rewrite parts of this in Clojure/ClojureScript.
3. Integrate the UI back into Deuce proper / revisit the best Emacs Lisp strategy.


### Rejected Approaches

In `gap.js` there's a Gap buffer implementation superseded the `rope.js`. In `paging.js` there's a complicated more "intelligent" version of the client which intuitively felt like the wrong approach, but had to be explored a bit. `deuce.js` is a naive client version which works directly with the DOM and no server, basically a client-first editor eventually depending on node-webkit. `render-spikes.js` is similar but focusing on viewing and has no editing.

All spikes have been mined for ideas in the current `components` version.

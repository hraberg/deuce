#!/usr/bin/env node
/*jslint node: true regexp: true nomen: true */

'use strict';

// http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf

// C: https://github.com/ivmai/bdwgc/blob/master/cord/cordbscs.c
// Common Lisp: https://github.com/Ramarren/ropes
// JavaScript: https://github.com/component/rope
// Java: https://code.google.com/p/ropes/
// Go: https://github.com/vinzmay/go-rope

var inspect = require('util').inspect,
    assert = require('assert');

function isString(x) {
    return typeof x === 'string' || x instanceof String;
}

var LENGTH = 0,
    LINES = 1,
    DEPTH = 2,
    WEIGHTS = 0,
    LEFT = 1,
    RIGHT = 2,
    SHORT_LIMIT = 16,
    MAX_DEPTH = 48;

function ropeToString(a) {
    if (!a) {
        return '';
    }
    var l = a[LEFT],
        r = a[RIGHT];
    if (isString(l)) {
        return l;
    }
    return ropeToString(l) + ropeToString(r);
}

function isLeaf(a) {
    return a && a[WEIGHTS][DEPTH] === 0;
}

function length(a) {
    if (a) {
        return a[WEIGHTS][LENGTH] + length(a[RIGHT]);
    }
    return 0;
}

var splitLinesPattern = /^.*((\r\n|\n|\r))/gm;

function newlines(a) {
    if (a) {
        return a[WEIGHTS][LINES] + newlines(a[RIGHT]);
    }
    return 0;
}

function depth(a) {
    if (a && !isLeaf(a)) {
        return Math.max(a[WEIGHTS][DEPTH], depth(a[RIGHT]) + 1);
    }
    return 0;
}

function weights(a) {
    if (a) {
        var aws = a[WEIGHTS],
            rws = weights(a[RIGHT]);
        return [aws[LENGTH] + rws[LENGTH], aws[LINES] + rws[LINES], Math.max(aws[DEPTH], rws[DEPTH]) + 1];
    }
    return [0, 0, 0];
}

function leaf(s) {
    return [[s.length, (s.match(splitLinesPattern) || '').length, 0], s];
}

function toRope(a) {
    return isString(a) ? leaf(a) : a;
}

function walk(a, f) {
    if (!a) {
        return;
    }
    return f(a);
    if (!isLeaf(a)) {
        return walk(a[LEFT], f);
        return walk(a[RIGHT], f);
    }
}

// this is from https://code.google.com/p/ropes/
function merge(leaves, start, end) {
    var range = end - start;
    switch (range) {
    case 1:
        return leaves[start];
    case 2:
        return cat(leaves[start], leaves[start + 1]);
    default:
        var middle = start - range / 2;
        return cat(merge(leaves, start, middle), merge(leaves, middle, end));
    }
}

function balance(a) {
    var leaves = [];
    walk(a, function (b) {
        if (isLeaf(b)) {
            leaves.push(b);
        }
    });
    return merge(leaves, 0, leaves.length);
}

function cat(a, b) {
    a = toRope(a);
    if (b) {
        var c = [weights(a), a, toRope(b)];
        if (length(c) <= SHORT_LIMIT) {
            return leaf(ropeToString(c));
        }
        if (depth(c) >= MAX_DEPTH) {
            return balance(c);
        }
        return c;
    }
    return [weights(a), a];
}

function rotateLeft(a) {
    var r = a[RIGHT];
    if (isLeaf(r) || !r) {
        return a;
    }
    return cat(cat(a[LEFT], r[LEFT]), r[RIGHT]);
}

function rotateRight(a) {
    var l = a[LEFT];
    if (isLeaf(l) || !l) {
        return a;
    }
    return cat(l[LEFT], cat(l[RIGHT], a[RIGHT]));
}

function index(a, i, ofLine) {
    if (!a) {
        return;
    }
    var w = a[WEIGHTS][ofLine ? LINES : LENGTH],
        l = a[LEFT],
        r = a[RIGHT];
    if (isString(l)) {
        if (ofLine) {
            return l.match(splitLinesPattern)[i];
        }
        return l[i];
    }
    if (w <= i) {
        return index(r, i - w, ofLine);
    }
    return index(l, i, ofLine);
}

// http://stackoverflow.com/a/22028152
function split(a, i, atLine) {
    var w = a[WEIGHTS][atLine ? LINES : LENGTH],
        l = a[LEFT],
        r = a[RIGHT],
        s;
    if (isString(l)) {
        if (atLine) {
            s = l.match(splitLinesPattern);
            return [leaf(s.splice(0, i).join('')), leaf(s.splice(i).join(''))];
        }
        return [leaf(l.substring(0, i)), leaf(l.substring(i))];
    }
    if (i < w) {
        s = split(l, i, atLine);
        return [s[0], cat(s[1], r)];
    }
    if (i > w) {
        s = split(r, i - w, atLine);
        return [cat(l, s[0]), s[1]];
    }
    return [l, r];
}

function lines(a, i, j) {
    var s = split(a, i, true)[1];
    return j ? split(s, j, true)[0] : s;
}

function fromStrings(ss) {
    return ss.reduce(function (r, s) {
        var n = leaf(s);
        if (!r) {
            return n;
        }
        return cat(r, n);
    }, null);
}

function insert(a, i, b, line) {
    var s = split(a, i, line);
    return cat(cat(s[0], b), s[1]);
}

function offsetOfLine(a, i) {
    return length(split(a, i, true)[0]);
}

function lineAtOffset(a, i) {
    return newlines(split(a, i + 1)[0]);
}

function deleteRange(a, i, j, lines) {
    var s = split(a, i, lines);
    return cat(s[0], split(s[1], j - i, lines)[1]);
}

function subs(a, i, j, lines) {
    var s = split(a, i, lines);
    return j ? split(s[1], j - i, lines)[0] : s[1];
}

// function *iterator(a) {
//     if (!a) {
//         return;
//     }
//     var l = a[1],
//         r = a[2];
//     if (isString(l)) {
//         yield *l;
//         return;
//     }
//     yield *iterator(l);
//     yield *iterator(r);
// }

// Example from http://en.wikipedia.org/wiki/Rope_(data_structure)
var example = cat(cat(cat('hello ', 'my '), cat(cat('na', 'me i'), cat('s', ' Simon'))));

function logInspect(o, depth) {
    console.log(inspect(o, false, depth || 10));
}

function debug(a, i) {
    var s = split(a, i);
    logInspect(a);
    logInspect(s[0]);
    logInspect(s[1]);
}

function bufferLines(s) {
    return s.match(/^.*((\r\n|\n|\r)|$)/gm);
}

function logTime(label, f) {
    console.time(label);
    try {
        return f();
    } finally {
        console.timeEnd(label);
    }
}

// https://github.com/ivmai/bdwgc/blob/master/cord/tests/cordtest.c

var x = toRope('ab');
assert(isLeaf(x));
assert.equal(depth(x), 0);
assert.equal(ropeToString(x), 'ab');
assert.equal(ropeToString(subs(x, 0, 1)), 'a');
assert.equal(ropeToString(subs(x, 1)), 'b');
assert.equal(length(x), 2);
assert.equal(newlines(x), 0);

x = cat(x, x);
assert(isLeaf(x));
assert.equal(depth(x), 0);
assert.equal(ropeToString(x), 'abab');
assert.equal(ropeToString(subs(x, 1, 3)), 'ba');
assert.equal(length(x), 4);
assert.equal(newlines(x), 0);

assert.equal(length(cat(x, x)), 8);
assert.equal(length(cat(cat(x, x), 'c')), 9);

var i;
for (i = 1; i < 16; i += 1) {
    x = cat(x, x);
}
x = cat(x, 'c');
assert(!isLeaf(x));
assert.equal(depth(x), 14);
assert.equal(length(x), 128 * 1024 + 1);

var y = subs(x, 1023, 5);
assert(isLeaf(y));
assert.equal(ropeToString(y), 'babab');

y = subs(x, 1024, 8);
assert(isLeaf(y));
assert.equal(ropeToString(y), 'abababab');

y = subs(x, 128 * 1024 - 1, 8);
assert(isLeaf(y));
assert.equal(ropeToString(y), 'bc');

x = balance(x);
assert.equal(length(x), 128 * 1024 + 1);

y = subs(x, 1023, 5);
assert(isLeaf(y));
assert.equal(ropeToString(y), 'babab');

logInspect(index(example, 10));
logInspect(length(example));
logInspect(depth(example));
logInspect(newlines(cat('Hello\n', 'World\n')));
logInspect(cat('Hello\n', 'World\n'));
logInspect(depth(cat('Hello\n', 'World\n')));
logInspect(lines(cat('Hello Ruthless\n', 'World\n'), 1));
logInspect(index(cat('Hello Ruthless\n', 'World\n'), 1, true));
logInspect(lines(cat('Hello Ruthless\n', 'World\n'), 0, 2));
logInspect(length(cat('Hello Ruthless\n', 'World\n')));
logInspect(offsetOfLine(cat('Hello Ruthless\n', 'World\n'), 2));
logInspect(index(cat('Hello Ruthless\n', 'World\n'), 0));
logInspect(index(cat('Hello Ruthless\n', 'World\n'), 15));
logInspect(index(cat('Hello Ruthless\n', 'World\n'), 13));
logInspect(lineAtOffset(cat('Hello Ruthless\n', 'World\n'), 17));
logInspect(inspect(insert(cat('Hello Ruthless\n', 'World\n'), 1, 'Space\n', true), false, 10));
logInspect(weights(example));
logInspect(ropeToString(example));

var simple = cat('hello ', 'my ');

console.log(ropeToString(simple));
debug(simple, 2);
debug(example, 11);

var fs = require('fs');

fs.readFile(__dirname + '/../etc/tutorials/TUTORIAL', 'utf8', function (err, data) {
    if (err) {
        return console.log(err);
    }
    var file,
        linesInFile,
        r,
        s;

    file = logTime('joining', function () { return new Array(10).join(data + '\n'); });
    linesInFile = logTime('split lines', function () { return bufferLines(file); });
    console.log('lines ' + linesInFile.length, (Math.round(file.length / (1024 * 1024) * 100) / 100) + 'Mb');
    r = logTime('concat', function () { return fromStrings(linesInFile); });
    console.log(length(r));
    r = logTime('insert', function () { return insert(r, Math.floor(length(r) / 2), 'Hello World'); });
    console.log(length(r));
    r = logTime('delete', function () { return deleteRange(r, Math.floor(length(r) / 2), (length(r) / 2) + 100); });
    console.log(length(r));
    console.log(logTime('index', function () { return index(r, Math.floor(length(r) / 2)); }));
    s = logTime('split', function () { return split(r, Math.floor(length(r) / 2)); });
    console.log(length(s[0]), length(s[1]));
});

#!/usr/bin/env node
/*jslint node: true regexp: true nomen: true */

'use strict';

// http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf

// C: https://github.com/ivmai/bdwgc/blob/master/cord/cordbscs.c
// Common Lisp: https://github.com/Ramarren/ropes
// JavaScript: https://github.com/component/rope
// Java: https://code.google.com/p/ropes/
// Go: https://github.com/vinzmay/go-rope
// Scheme: https://bitbucket.org/evhan/rope

var SHORT_LIMIT = 16;
var LINES_PATTERN = /^.*((\r\n|\n|\r))/gm;

function Rope(left, right) {
    this.left = left;
    this.right = right || '';
    this.weight = left.length;
    this.memoize();
}

Rope.prototype.memoize = function () {
    this.length = this.left.length + this.right.length;
    this.depth = Math.max(this.left.depth || 0, this.right.depth || 0) + 1;
    this.newlines = [this.left, this.right].map(function (x) {
        return x.newlines || (x.match(LINES_PATTERN) || []).length;
    }).reduce(function (x, y) { return x + y; });
};

Rope.prototype.toString = function () {
    return this.left.toString() + this.right.toString();
};

Rope.prototype.charAt = function (index) {
    if (index < this.weight) {
        return this.left.charAt(index);
    }
    return this.right.charAt(index - this.weight);
};

Rope.prototype.lineAtChild = function (child, index) {
    if (child.lineAt) {
        return child.lineAt(index);
    }
    return (child.slice(0, index).match(LINES_PATTERN) || []).length + 1;

};

Rope.prototype.lineAt = function (index) {
    if (index < 0 || index > this.length) {
        return -1;
    }
    if (index < this.weight) {
        return this.lineAtChild(this.left, index);
    }
    return this.lineAtChild(this.right, index - this.weight) + (this.left.newlines || 0) + 1;
};

Rope.prototype.indexOfLineInChild = function (child, line) {
    if (child.indexOfLine) {
        return child.indexOfLine(line);
    }
    return child.match(LINES_PATTERN).slice(0, line - 1).join('').length;
};

Rope.prototype.indexOfLine = function (line) {
    if (line === this.newlines + 1) {
        return this.length;
    }
    if (line < 1 || line > this.newlines) {
        return -1;
    }
    if (line <= this.left.newlines || !this.left.newlines) {
        return this.indexOfLineInChild(this.left, line);
    }
    return this.indexOfLineInChild(this.right, line) + this.weight;
};

Rope.prototype.match = function (regexp) {
    return this.toString().match(regexp);
};

Rope.prototype.concat = function () {
    var acc = this, i, x;
    for (i = 0; i < arguments.length; i += 1) {
        x = arguments[i];
        if (acc.length + x.length < SHORT_LIMIT) {
            acc = new Rope(acc + x);
        } else {
            acc = new Rope(acc, x);
        }
    }
    return acc;
};

Rope.prototype.slice = function (beginSlice, endSlice) {
    var left, right;
    endSlice = endSlice || this.length;
    if (endSlice < 0) {
        endSlice += this.length;
    }
    if (beginSlice < this.weight) {
        left = this.left.slice(beginSlice).slice(0, Math.min(endSlice, this.weight));
    }
    if (endSlice > this.weight) {
        right = this.right.slice(Math.max(0, beginSlice - this.weight), endSlice - this.weight);
    }
    if (left) {
        return new Rope(left, right);
    }
    return new Rope(right);
};

Rope.prototype.insert = function (offset, str) {
    return this.slice(0, offset).concat(str).concat(this.slice(offset));
};

Rope.prototype.del = function (start, end) {
    return this.slice(0, start).concat(this.slice(end));
};

// Rope.prototype[Symbol.iterator] = function* () {
//     yield *this.left;
//     yield *this.right;
// }

var assert = require('assert');

var r = new Rope('Hello', 'World');
assert.equal(r.length, 10);
assert.equal(r.toString(), 'HelloWorld');
assert.equal(r + r, 'HelloWorldHelloWorld');
assert.equal(r.charAt(0), 'H');
assert.equal(r.charAt(9), 'd');

r = r.concat(r);
assert.equal(r.length, 20);
assert.equal(r.toString(), 'HelloWorldHelloWorld');
assert.equal(r.charAt(0), 'H');
assert.equal(r.charAt(19), 'd');

assert.equal(new Rope('Hello').concat('World').right.toString(), '');
assert.equal(new Rope('Hello').concat('World').constructor, Rope);

assert.equal(new Rope('Hello', 'World').slice(0, 5).toString(), 'Hello');
assert.equal(new Rope('Hello', 'World').slice(5).toString(), 'World');
assert.equal(new Rope('Hello', 'World').slice(3, 8).toString(), 'loWor');

assert.equal(new Rope('Hello', 'World').insert(3, 'Space').toString(), 'HelSpaceloWorld');
assert.equal(new Rope('Hello', 'World').del(3, 8).toString(), 'Helld');

assert.equal(new Rope('Hello').newlines, 0);
assert.equal(new Rope('Hello').depth, 1);
assert.equal(new Rope('Hello\n', 'World\n').newlines, 2);
assert.equal(new Rope(new Rope('Hello\n'), new Rope('World\n')).depth, 2);

assert(new Rope('Hello', 'World').match('Hello'));
assert(!new Rope('Hello', 'World').match('Space'));

assert.equal(new Rope('Hello\n', 'World\n').lineAt(-1), -1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(0), 1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(8), 2);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(12), 3);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(13), -1);

assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(1), 0);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(2), 6);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(3), 12);

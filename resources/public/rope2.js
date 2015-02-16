#!/usr/bin/env node
/*jslint node: true regexp: true */

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

var Rope, RopeString;

function mixin(target, source, methods) {
    methods.forEach(function (m) {
        target.prototype[m] = source.prototype[m];
    });
}

function toRope(x) {
    if (x instanceof Rope || x instanceof RopeString) {
        return x;
    }
    return new RopeString((x || '').toString());
}

function Rope(left, right) {
    this.left = toRope(left);
    this.right = toRope(right);
    this.weight = left.length;
    this.memoize();
}

mixin(Rope, String, ['match', 'indexOf']);

Rope.prototype.memoize = function () {
    this.length = this.left.length + this.right.length;
    this.depth = Math.max(this.left.depth, this.right.depth) + 1;
    this.newlines = this.left.newlines + this.right.newlines;
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

Rope.prototype.lineAt = function (index) {
    if (index < this.weight) {
        return this.left.lineAt(index);
    }
    var line = this.right.lineAt(index - this.weight);
    return line === -1 ? -1 : line + this.left.newlines;
};

Rope.prototype.indexOfLine = function (line) {
    if (line <= this.left.newlines + 1) {
        return this.left.indexOfLine(line);
    }
    var index = this.right.indexOfLine(line - this.left.newlines);
    return index === -1 ? -1 : index + this.weight;
};

Rope.prototype.concat = function () {
    return [].slice.call(arguments).reduce(function (acc, x) {
        if (acc.length + x.length < SHORT_LIMIT) {
            return new RopeString(acc + x);
        }
        return new Rope(acc, x);
    }, this);
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
    if (endSlice >= this.weight) {
        right = this.right.slice(Math.max(0, beginSlice - this.weight), endSlice - this.weight);
    }
    if (left && right) {
        return toRope(left).concat(right);
    }
    if (left) {
        return toRope(left);
    }
    return toRope(right);
};

Rope.prototype.line = function (line) {
    return this.slice(this.indexOfLine(line), this.indexOfLine(line + 1));
};

Rope.prototype.insert = function (offset, str) {
    return this.slice(0, offset).concat(str).concat(this.slice(offset));
};

Rope.prototype.del = function (start, end) {
    return this.slice(0, start).concat(this.slice(end));
};

function RopeString(s) {
    this.s = s;
    this.length = s.length;
    this.depth = 0;
    this.newlines = (s.match(LINES_PATTERN) || []).length;
}

mixin(RopeString, String, ['charAt', 'match', 'indexOf']);
mixin(RopeString, Rope, ['concat', 'insert', 'del', 'line']);

RopeString.prototype.toString = function () {
    return this.s;
};

RopeString.prototype.slice = function (beginSlice, endSlice) {
    return new RopeString(this.s.slice(beginSlice, endSlice));
};

RopeString.prototype.lineAt = function (index) {
    if (index < 0 || index > this.length) {
        return -1;
    }
    return (this.s.slice(0, index).match(LINES_PATTERN) || []).length + 1;
};

RopeString.prototype.indexOfLine = function (line) {
    if (line < 1 || line > this.newlines + 1) {
        return -1;
    }
    if (line === this.newlines + 1) {
        return this.length;
    }
    var m = this.s.match(LINES_PATTERN);
    return m ? m.slice(0, line - 1).join('').length : -1;
};

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

assert.equal(new Rope('Hello').concat('World').constructor, RopeString);

assert.equal(new Rope('Hello', 'World').slice(0, 5), 'Hello');
assert.equal(new Rope('Hello', 'World').slice(5), 'World');
assert.equal(new Rope('Hello', 'World').slice(3, 8), 'loWor');
assert.equal(new Rope('Hello', 'World').slice(3, 8).constructor, RopeString);

assert.equal(new Rope('Hello', 'World').insert(3, 'Space'), 'HelSpaceloWorld');
assert.equal(new Rope('Hello', 'World').del(3, 8), 'Helld');

assert.equal(new Rope('Hello').newlines, 0);
assert.equal(new Rope('Hello').depth, 1);
assert.equal(new Rope('Hello\n', 'World\n').newlines, 2);
assert.equal(new Rope(new Rope('Hello\n'), new Rope('World\n')).depth, 2);

assert(new Rope('Hello', 'World').match('Hello'));
assert(!new Rope('Hello', 'World').match('Space'));
assert(new Rope('Hello', 'World').indexOf('oW'), 4);

assert.equal(new RopeString('HelloWorld').charAt(0), 'H');
assert.equal(new RopeString('HelloWorld').slice(3, 8), 'loWor');
assert.equal(new RopeString('HelloWorld').slice(3, 8).constructor, RopeString);
assert.equal(new RopeString('Hello').concat('World'), 'HelloWorld');
assert.equal(new RopeString('HelloWorld').lineAt(0), 1);

assert.equal(new RopeString('HelloWorld').insert(3, 'Space'), 'HelSpaceloWorld');
assert.equal(new RopeString('HelloWorld').insert(3, 'A longer String').constructor, Rope);
assert.equal(new RopeString('HelloWorld').del(3, 8), 'Helld');

assert.equal(new Rope('Hello\n', 'World\n').lineAt(-1), -1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(0), 1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(8), 2);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(12), 3);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(13), -1);

assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(1), 0);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(2), 6);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(3), 12);

assert.equal(new Rope('Hello\n', 'World\n').line(2), 'World\n');
assert.equal(new Rope('Hello\n', 'World\n').concat('Space').line(2), 'World\n');

/*jshint node: true, esnext: true */

// http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf

// C: https://github.com/ivmai/bdwgc/blob/master/cord/cordbscs.c
// Common Lisp: https://github.com/Ramarren/ropes
// JavaScript: https://github.com/component/rope
// Java: https://code.google.com/p/ropes/
// Go: https://github.com/vinzmay/go-rope
// Scheme: https://bitbucket.org/evhan/rope
// Haskell: https://github.com/yi-editor/yi-rope

'use strict';

function mixin(target, source, methods) {
    methods.forEach(function (m) {
        target.prototype[m] = source.prototype[m];
    });
}

function Rope(left, right) {
    this.left = Rope.toRope(left);
    this.right = Rope.toRope(right);
    this.weight = left.length;
    this.length = this.left.length + this.right.length;
    this.depth = Math.max(this.left.depth, this.right.depth) + 1;
    this.newlines = this.left.newlines + this.right.newlines;
}

var RopeString, RopeBuffer;

Rope.SHORT_LIMIT = 16;
Rope.LONG_LIMIT = 1024;
Rope.LINES_PATTERN = /^.*(\r\n?|\n)/gm;

Rope.EMPTY = new RopeString('');
Rope.EMPTY.concat = (rope) => Rope.toRope(rope);

Rope.toRope = (x) => {
    if (x instanceof Rope || x instanceof RopeString || x instanceof RopeBuffer) {
        return x;
    }
    if (x instanceof Array) {
        if (x.length === 0) {
            return Rope.EMPTY;
        }
        return x.reduce((left, right) => {
            return Rope.toRope(left).concat(Rope.toRope(right));
        });
    }
    return x === undefined ? Rope.EMPTY : new RopeString(x).balance();
};

mixin(Rope, String, ['match', 'indexOf']);

Rope.prototype.toString = () =>
    this.left.toString() + this.right.toString();

Rope.prototype.charAt = (index) => {
    if (index < this.weight) {
        return this.left.charAt(index);
    }
    return this.right.charAt(index - this.weight);
};

Rope.prototype.lineAt = (index) => {
    if (index < this.weight) {
        return this.left.lineAt(index);
    }
    let line = this.right.lineAt(index - this.weight);
    return line === -1 ? -1 : line + this.left.newlines;
};

Rope.prototype.indexOfLine = (line) => {
    if (line <= this.left.newlines) {
        return this.left.indexOfLine(line);
    }
    let index = this.right.indexOfLine(line - this.left.newlines);
    return index === -1 ? -1 : index + this.weight;
};

Rope.prototype.isBalanced = () =>
    this.length >= Rope.fib(this.depth + 2);

Rope.prototype.rotateLeft = () => {
    if (this.right instanceof Rope) {
        return new Rope(new Rope(this.left, this.right.left), this.right.right);
    }
    return this;
};

Rope.prototype.rotateRight = () => {
    if (this.left instanceof Rope) {
        return new Rope(this.left.left, new Rope(this.left.right, this.right));
    }
    return this;
};

Rope.prototype.balanceRotate = () => {
    if (this.left.depth > this.right.depth) {
        return this.rotateRight();
    }
    if (this.right.depth > this.left.depth) {
        return this.rotateLeft();
    }
    return this;
};

Rope.fib = (n) => {
    if (n <= 1) {
        return n;
    }
    return Rope.fib(n - 1) + Rope.fib(n - 2);
};

Rope.fib = ((f) => {
    let memo = [];
    return (n) => {
        if (memo[n] === undefined) {
            memo[n] = f(n);
        }
        return memo[n];
    };
})(Rope.fib);

// based on this version in Common Lisp: https://github.com/Ramarren/ropes
Rope.balanceFibStep = (acc, rope) => {
    let n = 0,
        maxLength = Rope.fib(n + 2),
        recur = true;
    while (recur) {
        if (!acc[n] && rope.length <= maxLength) {
            acc[n] = rope;
            return acc;
        } else if (acc[n]) {
            rope = new Rope(acc[n], rope);
            acc[n] = undefined;
        } else if (rope.length > maxLength) {
            n += 1;
            maxLength = Rope.fib(n + 2);
        } else {
            throw new Error('Invalid state:', acc, n);
        }
    }
};

Rope.prototype.balanceFib = (force, postConditions) => {
    if (this.isBalanced() && !force) {
        return this;
    }
    let balanced = Rope.toRope(this.reduce(Rope.balanceFibStep, []).reverse());
    if (postConditions && balanced.toString() !== this.toString()) {
        throw new Error('not same after balancing: \'' + this.toString().slice(0, 64) +
                        '\' ' + this.length + ' \'' + balanced.toString().slice(0, 64) + '\' ' + balanced.length);
    }
    return balanced;
};

Rope.prototype.balance = Rope.prototype.balanceFib;

Rope.prototype.concat = (rope) => {
    if (rope === Rope.EMPTY || !rope) {
        return this;
    }
    if (this.length + rope.length < Rope.SHORT_LIMIT) {
        return new RopeString(this.toString() + rope.toString());
    }
    return new Rope(this, rope).balance();
};

Rope.prototype.slice = (beginSlice, endSlice) => {
    let left, right;
    endSlice = endSlice === undefined ? this.length : endSlice;
    if (beginSlice < 0) {
        beginSlice += this.length;
    }
    if (endSlice < 0) {
        endSlice += this.length;
    }
    if (beginSlice === endSlice) {
        return Rope.EMPTY;
    }
    if (beginSlice === 0 && endSlice === this.length) {
        return this;
    }
    if (beginSlice < this.weight) {
        left = this.left.slice(beginSlice, Math.min(endSlice, this.weight));
    }
    if (endSlice >= this.weight) {
        right = this.right.slice(Math.max(0, beginSlice - this.weight), endSlice - this.weight);
    }
    if (left && right) {
        return left.concat(right);
    }
    return left || right;
};

Rope.prototype.lines = (startLine, endLine) =>
    this.slice(this.indexOfLine(startLine), endLine !== undefined &&
               endLine <= this.newlines ? this.indexOfLine(endLine) : this.length);

Rope.prototype.line = (line) => this.lines(line, line + 1);

Rope.prototype.insert = (offset, str) =>
    this.slice(0, offset).concat(str).concat(this.slice(offset));

Rope.prototype.del = (start, end) =>
    this.slice(0, start).concat(this.slice(end));

Rope.prototype.reduce = (f, acc) => {
    let todo = [this];
    while (todo.length > 0) {
        let next = todo.pop();
        if (next instanceof Rope) {
            todo.push(next.right);
            todo.push(next.left);
        } else {
            acc = f(acc, next);
        }
    }
    return acc;
};

function RopeString(s) {
    this.s = s.toString();
    this.length = s.length;
    this.depth = 0;
}

mixin(RopeString, String, ['charAt', 'match', 'indexOf']);
mixin(RopeString, Rope, ['concat', 'insert', 'del', 'lines', 'line', 'reduce']);

Object.defineProperty(RopeString.prototype, 'newlines', {
    enumerable: true,
    get: () => {
        if (this._newlines === undefined) {
            this._newlines = (this.match(Rope.LINES_PATTERN) || []).length;
        }
        return this._newlines;
    }
});

RopeString.prototype.toString = () => this.s;

RopeString.prototype.slice = (beginSlice, endSlice) => {
    return new RopeString(this.s.slice(beginSlice, endSlice)).balance();
};

RopeString.prototype.lineAt = (index) => {
    if (index < 0 || index > this.length) {
        return -1;
    }
    return (this.toString().slice(0, index).match(Rope.LINES_PATTERN) || []).length;
};

RopeString.prototype.indexOfLine = (line) => {
    if (line < 0 || line > this.newlines) {
        return -1;
    }
    return (this.match(Rope.LINES_PATTERN) || []).slice(0, line).join('').length;
};

RopeString.prototype.isBalanced = () =>
    this.length < Rope.LONG_LIMIT;

RopeString.prototype.balance = () => {
    if (this.length === 0) {
        return Rope.EMPTY;
    }
    if (this.isBalanced()) {
        return this;
    }
    let middle = Math.floor(this.length / 2);
    return this.slice(0, middle).concat(this.slice(middle, this.length));
};

// Assumes ASCII.

try {
    const fs = require('fs'),
          mmap = require('mmap.js');

    Rope.MMAP_THRESHOLD = 16 * 1024;
    Rope.openSync = (file) => {
        let fd = fs.openSync(file, 'r');
        try {
            let length = fs.fstatSync(fd).size;
            if (length > Rope.MMAP_THRESHOLD) {
                let buffer = mmap.alloc(length, mmap.PROT_READ, mmap.MAP_SHARED, fd, 0);
                return new RopeBuffer(buffer, 0, length);
            }
            return Rope.toRope(fs.readFileSync(file, {encoding: 'utf8'}));
        } finally {
            fs.closeSync(fd);
        }
    };
} catch (ignore) {
}

function RopeBuffer(buffer, start, end) {
    this.buffer = buffer;
    this.start = start;
    this.end = end;
    this.length = end - start;
    this.depth = 0;
    this.isStringBuffer = buffer.charAt !== undefined;
}

mixin(RopeBuffer, String, ['match', 'indexOf']);
mixin(RopeBuffer, Rope, ['concat', 'insert', 'del', 'lines', 'line', 'reduce']);
mixin(RopeBuffer, RopeString, ['indexOfLine', 'lineAt']);

RopeBuffer.prototype.toString = () =>
    this.buffer.slice(this.start, this.end).toString();

RopeBuffer.prototype.charAt = (index) => {
    if (index < 0 || index >= this.length) {
        return '';
    }
    if (this.isStringBuffer) {
        return this.buffer.charAt(index + this.start);
    }
    return String.fromCharCode(this.buffer[index + this.start]);
};

Object.defineProperty(RopeBuffer.prototype, 'newlines', {
    enumerable: true,
    get: () => {
        if (this._newlines === undefined) {
            let acc = 0;
            for (let i = this.start; i < this.end; i += 1) {
                if (this.charAt(i) === '\n') {
                    acc += 1;
                }
            }
            this._newlines = acc;
        }
        return this._newlines;
    }
});

RopeBuffer.prototype.slice = (beginSlice, endSlice) =>
    new RopeBuffer(this.buffer, (beginSlice || 0) + this.start, endSlice ? endSlice + this.start : this.end);


RopeBuffer.prototype.balance = () => this;

let assert;

try {
    assert = require('assert');
    module.exports.Rope = Rope;
    module.exports.RopeBuffer = RopeBuffer;
} catch (e) {
    assert = (x, y) => {
        if (x !== (y || x)) {
            throw new Error(x + ' == ' + y);
        }
    };
    assert.equal = (x, y) => {
        assert(''.concat(x), ''.concat(y));
    };
    assert.deepEqual = (x, y) => {
        assert.equal(JSON.stringify(x), JSON.stringify(y));
    };
}

assert.equal(Rope.fib(10), 55);
assert.equal(Rope.fib(20), 6765);
assert.equal(Rope.fib(100), 354224848179262000000);

let r = new Rope('Hello', 'World');
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
assert.equal(new Rope('Hello', 'World').insert(10, 'Space'), 'HelloWorldSpace');
assert.equal(new Rope('Hello', 'World').insert(0, 'Space'), 'SpaceHelloWorld');
assert.equal(new Rope('Hello', 'World').insert(-1, 'Space'), 'HelloWorlSpaced');
assert.equal(new Rope('Hello', 'World').del(3, 8), 'Helld');

assert.equal(Rope.EMPTY.newlines, 0);
assert.equal(new Rope('Hello').right, Rope.EMPTY);
assert.equal(new Rope('Hello').newlines, 0);
assert.equal(new Rope('Hello').depth, 1);
assert.equal(new Rope('Hello\n', 'World\n').newlines, 2);
assert.equal(new Rope(new Rope('Hello\n'), new Rope('World\n')).depth, 2);
assert.equal(new Rope(new Rope(new Rope('Hello\n'), new Rope('World\n')),
                      new Rope(new Rope('Hello\n'), new Rope('World\n'))).balance().length, 24);

assert(new Rope('Hello', 'World').match('Hello'));
assert(!new Rope('Hello', 'World').match('Space'));
assert(new Rope('Hello', 'World').indexOf('oW'), 4);

assert.equal(new RopeString('HelloWorld').charAt(0), 'H');
assert.equal(new RopeString('HelloWorld').slice(3, 8), 'loWor');
assert.equal(new RopeString('HelloWorld').slice(3, 8).constructor, RopeString);
assert.equal(new RopeString('Hello').concat('World'), 'HelloWorld');
assert.equal(new RopeString('HelloWorld').lineAt(0), 0);

assert.equal(new RopeString('HelloWorld').insert(3, 'Space'), 'HelSpaceloWorld');
assert.equal(new RopeString('HelloWorld').insert(3, 'A longer String').constructor, Rope);
assert.equal(new RopeString('HelloWorld').del(3, 8), 'Helld');

assert.equal(new Rope('Hello\n', 'World\n').lineAt(-1), -1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(0), 0);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(6), 1);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(12), 2);
assert.equal(new Rope('Hello\n', 'World\n').lineAt(13), -1);

assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(-1), -1);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(0), 0);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(1), 6);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(2), 12);
assert.equal(new Rope('Hello\n', 'World\n').indexOfLine(3), -1);

assert.equal(new Rope('Hello\n', 'World\n').lines(0, 1), 'Hello\n');
assert.equal(new Rope('Hello\n', 'World\n').lines(0), 'Hello\nWorld\n');
assert.equal(new Rope('Hello\n', 'World\n').lines(1, 2), 'World\n');
assert.equal(new Rope('Hello\n', 'World\n').lines(1, 3), 'World\n');
assert.equal(new Rope('Hello\n', 'World\n').lines(0, 0), '');
assert.equal(new Rope('Hello\n', 'World\n').lines(1, 1), '');
assert.equal(new Rope('Hello\n', 'World\n').concat('Space').lines(1, 2), 'World\n');

assert.equal(new RopeString('Hello\nWorld\n').lines(0, 1), 'Hello\n');
assert.equal(new RopeString('Hello\nWorld\n').lines(1), 'World\n');

assert.deepEqual(new Rope('Hello\n', 'World\n').concat('Space').reduce((acc, x) => {
    acc.push(x.toString());
    return acc;
}, []), ['Hello\n', 'World\n', 'Space']);

assert.deepEqual(new RopeString('HelloWorld').reduce((acc, x) => {
    acc.push(x.toString());
    return acc;
}, []), ['HelloWorld']);

let rb = new Rope('a', new Rope('bc', new Rope('d', 'ef')));
assert.equal(rb.depth, 3);
assert.equal(rb.length, 6);
assert(rb.isBalanced());

rb = rb.balance(true);
assert.equal(rb.depth, 2);
assert.equal(rb.left.depth, 1);
assert.equal(rb.right.depth, 1);
assert.equal(rb.length, 6);
assert(rb.isBalanced());
assert.equal(rb, 'abcdef');


rb = new Rope('The qui', new Rope('ck b', new Rope('rown', new Rope(' fox ju', new Rope('mps o', new Rope('ver the', new Rope(' lazy ', new Rope('do', 'g.'))))))));
assert(!rb.isBalanced());
let rbr = rb.balanceRotate();
assert.equal(rbr, 'The quick brown fox jumps over the lazy dog.');
assert(rbr.isBalanced());

let rbf = rb.balanceFib();
assert.equal(rbf, 'The quick brown fox jumps over the lazy dog.');
assert(rbf.isBalanced());

const Benchmark = require('benchmark').Benchmark;

function benchmark(text) {
    text = new Array(1000).join(text + '\n');

    let rope = Rope.toRope(text);
    function run(s) {
        s.on('cycle', function (event) {
            console.log(this.name, String(event.target));
        }).on('error', function (event) {
            console.log(String(event));
        }).on('complete', function () {
            let ratio = Math.round(1000 * (this.filter('fastest').pluck('hz')[0] / this.filter('slowest').pluck('hz')[0])) / 1000;
            console.log(this.filter('fastest').pluck('name') + ' is ' + Benchmark.formatNumber(ratio) + ' times faster');
            console.log('Rope', rope.length, rope.depth, rope.newlines);
            console.log('String', text.length, 0, (text.match(/\r\n?|\n/gm) || []).length);
        }).run();
    }
    function warnIfUnbalanced(x) {
        if (!x.isBalanced()) {
            console.log('WARN: unbalanced:', 'length:', x.length, 'depth:', x.depth, 'min:', Rope.fib(x.depth + 2));
        }
    }

    run(new Benchmark.Suite('newlines').add('Rope', () => {
        var idx = Math.floor(Math.random() * rope.length);
        return rope.slice(idx).newlines;
    }).add('String', () => {
        var idx = Math.floor(Math.random() * text.length);
        return (text.slice(idx).match(/\r\n?|\n/gm) || []).length;
    }));

    run(new Benchmark.Suite('charAt').add('Rope', () => {
        var idx = Math.floor(Math.random() * rope.length);
        rope.charAt(idx);
    }).add('String', () => {
        var idx = Math.floor(Math.random() * text.length);
        text.charAt(idx);
    }));

    run(new Benchmark.Suite('lineAt').add('Rope', () => {
        var start = Math.floor(Math.random() * rope.length),
            idx = Math.floor(Math.random() * (rope.length - start));
        rope.slice(start).lineAt(idx);
    }).add('String', () => {
        var start = Math.floor(Math.random() * text.length),
            idx = Math.floor(Math.random() * (text.length - start));
        return (text.slice(start).slice(0, idx).match(/\r\n?|\n/gm) || []).length;
    }));

    run(new Benchmark.Suite('del').add('Rope', () => {
        var prev = rope,
            start = Math.floor(Math.random() * rope.length),
            length = Math.floor(Math.random() * 1024);
        length = Math.min(length, rope.length - start);
        rope = rope.del(start, start + length);
        assert.equal(rope.constructor, Rope);
        assert.equal(rope.length, prev.length - length);
        warnIfUnbalanced(rope);
    }).add('String', () => {
        var start = Math.floor(Math.random() * text.length),
            length = Math.floor(Math.random() * 1024);
        text = text.slice(0, start).concat(text.slice(Math.min(text.length, start + length)));
    }));

    run(new Benchmark.Suite('insert').add('Rope', () => {
        var prev = rope,
            start = Math.floor(Math.random() * rope.length),
            length = Math.floor(Math.random() * 1024);
        rope = rope.insert(start, [].constructor(length + 1).join('x'));
        assert.equal(rope.constructor, Rope);
        assert.equal(rope.length, prev.length + length);
        warnIfUnbalanced(rope);
    }).add('String', () => {
        var start = Math.floor(Math.random() * text.length),
            length = Math.floor(Math.random() * 1024);
        text = text.slice(0, start).concat([].constructor(length + 1).join('x')).concat(text.slice(start));
    }));
}

if (Rope.openSync) {
    const path = require('path');
    let rf = Rope.openSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'));

    assert.equal(rf.charAt(0), 'E');
    assert.equal(rf.charAt(-1), '');
    assert.equal(rf.charAt(46571 + 1), '');
    assert.equal(rf.length, 46571);
    assert.equal(rf.toString().length, 46571);
    assert.equal(rf._newlines, undefined);
    assert.equal(rf.newlines, 1122);
    assert.equal(rf._newlines, 1122);
    assert.equal(rf.slice(6).charAt(0), 't');
    assert.equal(rf.slice(6).constructor, RopeBuffer);
    assert.equal(rf.slice(128, 256).length, 128);
    assert(/Emacs tutorial/.test(rf.toString()));
    assert.equal(rf.lines(2, 3), 'Emacs commands generally involve the CONTROL key (sometimes labeled\n');
    assert.equal(rf.lines(2, 3).constructor, RopeBuffer);

    assert.equal(rf.insert(2000, 'HelloWorld').length, 46571 + 10);
    assert.equal(rf.insert(2000, 'HelloWorld').constructor, Rope);
    assert.equal(rf.del(2000, 3000).length, 46571 - 1000);
    assert.equal(rf.del(2000, 3000).left.constructor, RopeBuffer);
    assert.equal(rf.del(2000, 3000).right.constructor, RopeBuffer);

    assert.equal(Rope.toRope(rf.toString()).slice(128, 256).length, 128);

    if (process.argv[1] === __filename) {
        benchmark(rf.toString());
    }
}

/*jslint node: true browser: true regexp: true stupid: true nomen: true */

// http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf

// C: https://github.com/ivmai/bdwgc/blob/master/cord/cordbscs.c
// Common Lisp: https://github.com/Ramarren/ropes
// JavaScript: https://github.com/component/rope
// Java: https://code.google.com/p/ropes/
// Go: https://github.com/vinzmay/go-rope
// Scheme: https://bitbucket.org/evhan/rope

'use strict';

function mixin(target, source, methods) {
    methods.forEach(function (m) {
        target.prototype[m] = source.prototype[m];
    });
}

function memoize(f) {
    var memo = {};
    return function () {
        var args = [].slice.call(arguments);
        if (memo[args] === undefined) {
            memo[args] = f.apply(f, args);
        }
        return memo[args];
    };
}

function Rope(left, right) {
    this.left = Rope.toRope(left);
    this.right = Rope.toRope(right);
    this.weight = left.length;
    this.length = this.left.length + this.right.length;
    this.depth = Math.max(this.left.depth, this.right.depth) + 1;
    this.newlines = this.left.newlines + this.right.newlines;
}

var RopeString, RopeFile;

Rope.SHORT_LIMIT = 16;
Rope.LONG_LIMIT = 1024;
Rope.LINES_PATTERN = /^.*(\r\n?|\n)/gm;
Rope.EMPTY = new RopeString('');
Rope.EMPTY.concat = function (rope) {
    return Rope.toRope(rope);
};

Rope.toRope = function (x) {
    if (x instanceof Rope || x instanceof RopeString || x instanceof RopeFile) {
        return x;
    }
    if (x instanceof Array) {
        return x.reduce(function (left, right) {
            return Rope.toRope(left).concat(Rope.toRope(right));
        });
    }
    return x === undefined ? Rope.EMPTY :  new RopeString(x).balance();
};

Rope.fib =  function (n) {
    if (n <= 1) {
        return 1;
    }
    return Rope.fib(n - 1) + Rope.fib(n - 2);
};
Rope.fib = memoize(Rope.fib);

mixin(Rope, String, ['match', 'indexOf']);

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
    if (line <= this.left.newlines) {
        return this.left.indexOfLine(line);
    }
    var index = this.right.indexOfLine(line - this.left.newlines);
    return index === -1 ? -1 : index + this.weight;
};

Rope.prototype.balance = function () {
    if (this.left.depth > this.right.depth) {
        return this.rotateRight();
    }
    if (this.right.depth > this.left.depth) {
        return this.rotateLeft();
    }
    return this;
};

Rope.prototype.rotateLeft = function () {
    if (this.right instanceof Rope) {
        return new Rope(new Rope(this.left, this.right.left), this.right.right);
    }
    return this;
};

Rope.prototype.rotateRight = function () {
    if (this.left instanceof Rope) {
        return new Rope(this.left.left, new Rope(this.left.right, this.right));
    }
    return this;
};

Rope.prototype.concat = function (rope) {
    if (rope === Rope.EMPTY || !rope) {
        return this;
    }
    if (this.length + rope.length < Rope.SHORT_LIMIT) {
        return new RopeString(this + rope);
    }
    return new Rope(this, rope).balance();
};

Rope.prototype.slice = function (beginSlice, endSlice) {
    var left, right;
    endSlice = endSlice === undefined ? this.length : endSlice;
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
        left = this.left.slice(beginSlice).slice(0, Math.min(endSlice, this.weight));
    }
    if (endSlice >= this.weight) {
        right = this.right.slice(Math.max(0, beginSlice - this.weight), endSlice - this.weight);
    }
    if (left && right) {
        return left.concat(right);
    }
    return left || right;
};

Rope.prototype.lines = function (startLine, endLine) {
    return this.slice(this.indexOfLine(startLine), endLine ? this.indexOfLine(endLine) : this.length);
};

Rope.prototype.insert = function (offset, str) {
    return this.slice(0, offset).concat(str).concat(this.slice(offset));
};

Rope.prototype.del = function (start, end) {
    return this.slice(0, start).concat(this.slice(end));
};

Rope.prototype.reduce = function (f, acc) {
    var todo = [this],
        next;
    while (todo.length > 0) {
        next = todo.pop();
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
mixin(RopeString, Rope, ['concat', 'insert', 'del', 'lines', 'reduce']);

Object.defineProperty(RopeString.prototype, 'newlines', {
    enumerable: true,
    get: function () {
        if (this._newlines === undefined) {
            this._newlines = (this.match(Rope.LINES_PATTERN) || []).length;
        }
        return this._newlines;
    }
});

RopeString.prototype.toString = function () {
    return this.s;
};

RopeString.prototype.slice = function (beginSlice, endSlice) {
    return new RopeString(this.s.slice(beginSlice, endSlice)).balance();
};

RopeString.prototype.lineAt = function (index) {
    if (index < 0 || index > this.length) {
        return -1;
    }
    return (this.toString().slice(0, index).match(Rope.LINES_PATTERN) || []).length;
};

RopeString.prototype.indexOfLine = function (line) {
    if (line < 0 || line > this.newlines) {
        return -1;
    }
    return this.match(Rope.LINES_PATTERN).slice(0, line).join('').length;
};

RopeString.prototype.balance = function () {
    if (this.length === 0) {
        return Rope.EMPTY;
    }
    if (this.length < Rope.LONG_LIMIT) {
        return this;
    }
    var middle = Math.floor(this.length / 2);
    return this.slice(0, middle).concat(this.slice(middle, this.length));
};

// Assumes ASCII.

try {
    var fs = require('fs'),
        mmap = require('mmap.js');

    Rope.MMAP_THRESHOLD = 16 * 1024;
    Rope.openSync = function (file) {
        var fd = fs.openSync(file, 'r'), length, buffer;
        try {
            length = fs.fstatSync(fd).size;
            if (length > Rope.MMAP_THRESHOLD) {
                buffer = mmap.alloc(length, mmap.PROT_READ, mmap.MAP_SHARED, fd, 0);
                return new RopeFile(buffer, 0, length);
            }
            return Rope.toRope(fs.readFileSync(file, {encoding: 'utf8'}));
        } finally {
            fs.closeSync(fd);
        }
    };
} catch (ignore) {
}

function RopeFile(buffer, start, end) {
    this.buffer = buffer;
    this.start = start;
    this.end = end;
    this.length = end - start;
    this.depth = 0;
}

mixin(RopeFile, String, ['match', 'indexOf']);
mixin(RopeFile, Rope, ['concat', 'insert', 'del', 'lines', 'reduce']);
mixin(RopeFile, RopeString, ['indexOfLine', 'lineAt']);

RopeFile.prototype.toString = function () {
    return this.buffer.slice(this.start, this.end).toString();
};

RopeFile.prototype.charAt = function (index) {
    if (index < 0 || index >= this.length) {
        return '';
    }
    return String.fromCharCode(this.buffer[index + this.start]);
};

Object.defineProperty(RopeFile.prototype, 'newlines', {
    enumerable: true,
    get: function () {
        if (this._newlines === undefined) {
            var i, acc = 0, nl = '\n'.charCodeAt(0);
            for (i = this.start; i < this.end; i += 1) {
                if (this.buffer[i] === nl) {
                    acc += 1;
                }
            }
            this._newlines = acc;
        }
        return this._newlines;
    }
});

RopeFile.prototype.slice = function (beginSlice, endSlice) {
    return new RopeFile(this.buffer, (beginSlice || 0) + this.start, endSlice ? endSlice + this.start : this.end);
};

RopeFile.prototype.balance = function () {
    return this;
};

var assert;

try {
    assert = require('assert');
    module.exports.Rope = Rope;
} catch (e) {
    assert = function (x, y) {
        if (x !== (y || x)) {
            throw new Error(x + ' == ' + y);
        }
    };
    assert.equal = function (x, y) {
        assert(''.concat(x), ''.concat(y));
    };
    assert.deepEqual = function (x, y) {
        assert.equal(JSON.stringify(x), JSON.stringify(y));
    };
}

assert.equal(Rope.fib(10), 89);
assert.equal(Rope.fib(20), 10946);
assert.equal(Rope.fib(100), 573147844013817200000);

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
assert.equal(new Rope('Hello\n', 'World\n').concat('Space').lines(1, 2), 'World\n');

assert.equal(new RopeString('Hello\nWorld\n').lines(0, 1), 'Hello\n');
assert.equal(new RopeString('Hello\nWorld\n').lines(1), 'World\n');

assert.deepEqual(new Rope('Hello\n', 'World\n').concat('Space').reduce(function (acc, x) {
    acc.push(x.toString());
    return acc;
}, []), ['Hello\n', 'World\n', 'Space']);

assert.deepEqual(new RopeString('HelloWorld').reduce(function (acc, x) {
    acc.push(x.toString());
    return acc;
}, []), ['HelloWorld']);

function stress(text) {
    text = new Array(1000).join(text + '\n');
    var rs = Rope.toRope(text), i, idx;
    console.log(rs.length, rs.depth, rs.newlines);
    console.time('slice str');
    console.log(text.slice(Math.floor(text.length / 2)).length);
    console.log(text.length, 0, (text.match(/\r\n?|\n/gm) || []).length);
    console.timeEnd('slice str');
    console.time('slice');
    console.log(rs.slice(Math.floor(rs.length / 2)).length);
    console.timeEnd('slice');
    for (i = 0; i < 10; i += 1) {
        console.time('insert');
        idx = Math.floor(rs.length / 2);
        rs = rs.insert(idx, 'HelloWorld');
        console.log(rs.length, rs.depth, rs.lineAt(idx));
        console.timeEnd('insert');
        console.time('insert str');
        text = text.slice(0, idx) + 'HelloWorld' + text.slice(idx);
        console.log(text.length, 0, (text.slice(0, idx).match(/\r\n?|\n/gm) || []).length);
        console.timeEnd('insert str');
        console.time('delete');
        idx = Math.floor(rs.length / 2);
        rs = rs.del(idx, idx + 1000);
        console.log(rs.length, rs.depth, rs.lineAt(idx));
        console.timeEnd('delete');
        console.time('delete str');
        text = text.slice(0, idx) + text.slice(idx + 1000);
        console.log(text.length, 0, (text.slice(0, idx).match(/\r\n?|\n/gm) || []).length);
        console.timeEnd('delete str');

        console.time('newlines');
        console.log(rs.length, rs.depth, rs.newlines);
        console.timeEnd('newlines');

        console.time('newlines str');
        console.log(text.length, 0, (text.match(/\r\n?|\n/gm) || []).length);
        console.timeEnd('newlines str');

        console.time('toString');
        console.log(rs.toString().length);
        console.timeEnd('toString');

        console.time('toString str');
        console.log(text.toString().length);
        console.timeEnd('toString str');

        assert.equal(rs.length, text.length);
    }
    assert(rs.toString() === text);
    assert(rs.newlines, (text.match(/\r\n?|\n/gm) || []).length);
}

function benchmark(text) {
    text = new Array(1000).join(text + '\n');

    var Benchmark = require('benchmark').Benchmark,
        rope = Rope.toRope(text);

    function run(s) {
        s.on('cycle', function (event) {
            console.log(String(event.target));
        }).on('complete', function () {
            console.log('Fastest is ' + this.filter('fastest').pluck('name'));
        }).run();
    }

    run(new Benchmark.Suite().add('Rope newlines', function () {
        var idx = Math.floor(Math.random() * rope.length);
        return rope.slice(idx).newlines;
    }).add('String newlines', function () {
        var idx = Math.floor(Math.random() * text.length);
        return (text.slice(idx).match(/\r\n?|\n/gm) || []).length;
    }));

    run(new Benchmark.Suite().add('Rope del', function () {
        var start = Math.floor(Math.random() * rope.length),
            end = Math.floor(Math.random() * (rope.length - start)) + start;
        rope = rope.del(start, end);
    }).add('String del', function () {
        var start = Math.floor(Math.random() * text.length),
            end = Math.floor(Math.random() * (text.length - start)) + start;
        text = text.slice(0, start).concat(text.slice(end));
    }));

    run(new Benchmark.Suite().add('Rope insert', function () {
        var start = Math.floor(Math.random() * rope.length),
            length = Math.floor(Math.random() * 1024);
        rope = rope.insert(start, [].constructor(length).join('x'));
    }).add('String insert', function () {
        var start = Math.floor(Math.random() * text.length),
            length = Math.floor(Math.random() * 1024);
        text = text.slice(0, start).concat([].constructor(length).join('x')).concat(text.slice(start));
    }));

    run(new Benchmark.Suite().add('Rope lineAt', function () {
        var idx = Math.floor(Math.random() * rope.length);
        rope.lineAt(idx);
    }).add('String lineAt', function () {
        var idx = Math.floor(Math.random() * rope.length);
        return (text.slice(0, idx).match(/\r\n?|\n/gm) || []).length;
    }));
}

try {
    window.setTimeout(function () {
        stress(document.querySelector('[data-filename=TUTORIAL]').textContent);
    }, 10000);
} catch (ignore) {
}

if (Rope.openSync) {
    var rf = Rope.openSync(__dirname + '/../etc/tutorials/TUTORIAL');

    assert.equal(rf.charAt(0), 'E');
    assert.equal(rf.charAt(-1), '');
    assert.equal(rf.charAt(46571 + 1), '');
    assert.equal(rf.length, 46571);
    assert.equal(rf.toString().length, 46571);
    assert.equal(rf._newlines, undefined);
    assert.equal(rf.newlines, 1122);
    assert.equal(rf._newlines, 1122);
    assert.equal(rf.slice(6).charAt(0), 't');
    assert.equal(rf.slice(6).constructor, RopeFile);
    assert(/Emacs tutorial/.test(rf.toString()));
    assert.equal(rf.lines(2, 3), 'Emacs commands generally involve the CONTROL key (sometimes labeled\n');
    assert.equal(rf.lines(2, 3).constructor, RopeFile);

    assert.equal(rf.insert(2000, 'HelloWorld').length, 46571 + 10);
    assert.equal(rf.insert(2000, 'HelloWorld').constructor, Rope);
    assert.equal(rf.del(2000, 3000).length, 46571 - 1000);
    assert.equal(rf.del(2000, 3000).left.constructor, RopeFile);
    assert.equal(rf.del(2000, 3000).right.constructor, RopeFile);

    benchmark(rf.toString());
}

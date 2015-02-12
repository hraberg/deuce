#!/usr/bin/env node
/*jslint node: true regexp: true nomen: true */

'use strict';

// http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf

function length(a) {
    if (a) {
        return a[0] + length(a[2]);
    }
    return 0;
}

function leaf(s) {
    return [s.length, s];
}

function concat(a, b) {
    return [length(a), a, b];
}

function isString(x) {
    return typeof x === 'string' || x instanceof String;
}

function index(a, i) {
    if (!a) {
        return;
    }
    var w = a[0],
        l = a[1],
        r = a[2];
    if (isString(l)) {
        return l[i];
    }
    if (w < i) {
        return index(r, i - w);
    }
    return index(l, i);
}

// http://stackoverflow.com/a/22028152
function split(a, i) {
    var w = a[0],
        l = a[1],
        r = a[2],
        s;
    if (isString(l)) {
        return [leaf(l.substring(0, i)), leaf(l.substring(i))];
    }
    if (i < w) {
        s = split(l, i);
        return [s[0], concat(s[1], r)];
    }
    if (i > a[0]) {
        s = split(r, i - w);
        return [concat(l, s[0]), s[1]];
    }
    return [l, r];
}

function ropeToString(a) {
    if (!a) {
        return '';
    }
    var l = a[1],
        r = a[2];
    if (isString(l)) {
        return l;
    }
    return ropeToString(l) + ropeToString(r);
}

function fromStrings(ss) {
    return ss.reduce(function (r, s) {
        var n = leaf(s);
        if (!r) {
            return n;
        }
        return concat(r, n);
    }, null);
}


function insert(a, i, b) {
    var s = split(a, i);
    if (isString(b)) {
        b = leaf(b);
    }
    return concat(concat(s[0], b), s[1]);
}

function deleteRange(a, i, j) {
    var s = split(a, i);
    return concat(s[0], split(s[1], j - i)[1]);
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
var example = [22, [9, [6, [6, 'hello '], [3, 'my ']], [6, [2, [2, 'na'], [4, 'me i']], [1, [1, 's'], [6, ' Simon']]]]];

function debug(a, i) {
    var s = split(a, i);
    console.log(JSON.stringify(a));
    console.log(JSON.stringify(s[0]));
    console.log(JSON.stringify(s[1]));
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

console.log(index(example, 10));
console.log(length(example));
console.log(ropeToString(example));

var simple = [6, [6, 'hello '], [3, 'my ']];

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

#!/usr/bin/env node
/*jslint node: true */

'use strict';

var assert = require('assert');

// not persistent, uses destructive updates to the buffer.
function GapBuffer(s) {
    this.buffer = s.split('');
    this.point = 0;
    this.start = 0;
    this.end = 0;
    this.grow();
}

GapBuffer.GROWTH_FACTOR = 0.5;

GapBuffer.prototype.toString = function () {
    return this.buffer.slice(0, this.start).concat(this.buffer.slice(this.end)).join('');
};

Object.defineProperty(GapBuffer.prototype, 'length', {
    get: function () {
        return this.buffer.length - (this.end - this.start);
    }
});

GapBuffer.prototype.grow = function () {
    var size = Math.round(this.length * GapBuffer.GROWTH_FACTOR),
        before = this.buffer.slice(0, this.start),
        gap = [].constructor(size),
        after = this.buffer.slice(this.end);

    this.buffer = before.concat(gap.concat(after));
    this.start = before.length;
    this.end = before.length + gap.length;

    return this;
};

GapBuffer.prototype.expect = function (expected) {
    if (typeof expected === 'string') {
        assert.equal(this.toString(), expected);
    } else {
        var that = this;
        assert.deepEqual(Object.keys(expected).reduce(function (m, k) {
            m[k] = that[k];
            return m;
        }, {}), expected);
    }
    return this;
};

GapBuffer.prototype.moveGapToPoint = function () {
    if (this.point === this.start) {
        return this;
    }
    var before = this.buffer.slice(0, this.start),
        gap = this.buffer.slice(this.start, this.end),
        after = this.buffer.slice(this.end),
        text = before.concat(after);
    before = text.slice(0, this.point);
    after = text.slice(this.point);

    this.buffer = before.concat(gap).concat(after);
    this.start = this.point;
    this.end = this.point + gap.length;

    return this;
};

GapBuffer.prototype.gotoChar = function (n) {
    this.point = Math.max(0, Math.min(n, this.length));
    return this;
};


GapBuffer.prototype.bobp = function () {
    return this.point === 0;
};

GapBuffer.prototype.eobp = function () {
    return this.point === this.length;
};

GapBuffer.prototype.forwardChar = function (n) {
    n = n || 1;
    var direction = n > 0 ? 1 : -1,
        oldPoint;
    n = Math.abs(n);
    while (n > 0) {
        oldPoint = this.point;
        this.gotoChar(this.point + direction);
        if (this.point === oldPoint) {
            break;
        }
        if (oldPoint === this.start) {
            if (direction > 0) {
                this.buffer[this.start] = this.buffer[this.end];
                delete this.buffer[this.end];
            } else {
                this.buffer[this.end - 1] = this.buffer[this.start - 1];
                delete this.buffer[this.start + direction];
            }
            this.start += direction;
            this.end += direction;
        }
        n -= 1;
    }
    return this;
};

GapBuffer.prototype.backwardChar = function (n) {
    return this.forwardChar(-n);
};

GapBuffer.prototype.insert = function (s) {
    this.moveGapToPoint();
    var i;
    for (i = 0; i < s.length; i += 1) {
        this.buffer[this.start] = s[i];
        this.start += 1;
        this.gotoChar(this.point + 1);
        if (this.start === this.end) {
            this.grow();
        }
    }
    return this;
};

GapBuffer.prototype.deleteChar = function (n) {
    this.moveGapToPoint();
    n = n || 1;
    var direction = n > 0 ? 1 : -1;
    n = Math.abs(n);
    while (n > 0) {
        if (direction > 0) {
            if (this.eobp()) {
                break;
            }
            delete this.buffer[this.end];
            this.end += direction;
        } else {
            if (this.bobp()) {
                break;
            }
            this.start += direction;
            this.gotoChar(this.point + direction);
            delete this.buffer[this.start];
        }
        n -= 1;
    }
    return this;
};

GapBuffer.prototype.backwardDeleteChar = function (n) {
    return this.deleteChar(-n);
};

var buffer = new GapBuffer('Hello World');

buffer.expect('Hello World').expect({point: 0, start: 0, end: 6})
    .gotoChar(2).expect({point: 2, start: 0, end: 6})
    .forwardChar(4).expect({point: 6})
    .backwardChar(4).expect({point: 2})
    .forwardChar(5).expect({point: 7})
    .forwardChar(5).expect({point: 11, start: 0, end: 6})
    .insert('Space!').expect('Hello WorldSpace!').expect({point: 17, start: 17, end: 26})
    .backwardChar(2).expect({point: 15, start: 15, end: 24})
    .deleteChar(2).expect('Hello WorldSpac').expect({start: 15, end: 26})
    .backwardDeleteChar(3).expect('Hello WorldS').expect({start: 12, end: 26})
    .forwardChar(50).expect({start: 12, end: 26})
    .gotoChar(5).expect({point: 5, start: 12, end: 26});

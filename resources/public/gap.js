#!/usr/bin/env node
/*jslint node: true */

'use strict';

var GROWTH_FACTOR = 0.5,
    FORWARD = 1,
    BACKWARD = -1;

// not persistent, uses destructive updates to the buffer.
function GapBuffer(s) {
    this.buffer = s.split('');
    this.point = 0;
    this.start = 0;
    this.end = 0;
    this.markRing = [];
    this.grow();
}

GapBuffer.prototype.toString = function () {
    return this.buffer.slice(0, this.start).concat(this.buffer.slice(this.end)).join('');
};

GapBuffer.prototype.offsetToIndex = function (offset) {
    if (offset < this.start) {
        return offset;
    }
    return offset + (this.end - this.start);
};

GapBuffer.prototype.charAt = function (n) {
    return this.buffer[this.offsetToIndex(n)];
};

GapBuffer.prototype.toArray = function () {
    return this.toString().split('');
};

Object.defineProperty(GapBuffer.prototype, 'length', {
    enumerable: true,
    get: function () {
        return this.buffer.length - (this.end - this.start);
    }
});

GapBuffer.prototype.grow = function () {
    var size = Math.round(this.length * GROWTH_FACTOR),
        before = this.buffer.slice(0, this.start),
        gap = [].constructor(size),
        after = this.buffer.slice(this.end);

    this.buffer = before.concat(gap.concat(after));
    this.start = before.length;
    this.end = before.length + gap.length;

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

GapBuffer.prototype.pushMark = function () {
    this.markRing.push(this.point);
    return this;
};

GapBuffer.prototype.popMark = function () {
    this.markRing.pop();
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
    var direction = n > 0 ? FORWARD : BACKWARD,
        oldPoint;
    n = Math.abs(n);
    while (n > 0) {
        oldPoint = this.point;
        this.gotoChar(this.point + direction);
        if (this.point === oldPoint) {
            break;
        }
        if (oldPoint === this.start) {
            if (direction === FORWARD) {
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
    return this.forwardChar(-(n || 1));
};

GapBuffer.prototype.insert = function (s) {
    if (this.markRing.length > 0) {
        this.deleteRegion();
    }
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

GapBuffer.prototype.deleteRegion = function (start, end) {
    start = start || this.point;
    if (!end && this.markRing.length > 0) {
        end = this.markRing.pop();
    } else if (!end) {
        return this;
    }

    this.gotoChar(Math.min(start, end));
    this.moveGapToPoint();
    this.end = this.offsetToIndex(Math.max(start, end));

    var i;
    for (i = this.start; i < this.end; i += 1) {
        delete this.buffer[i];
    }

    return this;
};

GapBuffer.prototype.deleteChar = function (n) {
    if (this.markRing.length > 0) {
        return this.deleteRegion();
    }
    this.moveGapToPoint();
    n = n || 1;
    var direction = n > 0 ? FORWARD : BACKWARD;
    n = Math.abs(n);
    while (n > 0) {
        if (direction === FORWARD) {
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
    return this.deleteChar(-(n || 1));
};

var assert = require('assert');

GapBuffer.prototype.expect = function (expected) {
    if (typeof expected === 'string') {
        assert.equal(this.toString(), expected);
    } else {
        var that = this;
        assert.deepEqual(Object.keys(expected).reduce(function (m, k) {
            m[k] = k.split('.').reduce(function (o, k) {
                return o[k];
            }, that);
            return m;
        }, {}), expected);
    }
    return this;
};

var text = 'Hello World',
    buffer = new GapBuffer(text),
    i;

assert.equal(buffer.length, text.length);
assert.deepEqual(text.split(''), buffer.toArray());
for (i = 0; i < text.length; i += 1) {
    assert.equal(buffer.charAt(i), text.charAt(i));
}

buffer.expect('Hello World').expect({point: 0, start: 0, end: 6, length: 11, 'buffer.length': 17})
    .gotoChar(2).expect({point: 2, start: 0, end: 6})
    .forwardChar(4).expect({point: 6})
    .backwardChar(4).expect({point: 2})
    .forwardChar(5).expect({point: 7})
    .forwardChar(5).expect({point: 11, start: 0, end: 6})
    .insert('Space!').expect('Hello WorldSpace!').expect({point: 17, start: 17, end: 26, 'buffer.length': 26})
    .backwardChar(2).expect({point: 15, start: 15, end: 24})
    .deleteChar(2).expect('Hello WorldSpac').expect({start: 15, end: 26})
    .backwardDeleteChar(3).expect('Hello WorldS').expect({start: 12, end: 26})
    .forwardChar(50).expect({start: 12, end: 26})
    .gotoChar(5).expect({point: 5, start: 12, end: 26})
    .pushMark().expect({markRing: [5]})
    .gotoChar(9).expect({point: 9, markRing: [5]})
    .deleteRegion().expect({point: 5, start: 5, end: 23, markRing: [], length: 8}).expect('HelloldS')
    .gotoChar(0).pushMark().expect({markRing: [0]})
    .forwardChar(3).expect({point: 3, markRing: [0]})
    .backwardDeleteChar(1).expect({point: 0, start: 0, end: 21, markRing: [], length: 5}).expect('loldS')
    .pushMark().expect({markRing: [0]})
    .forwardChar(2).insert('ABC').expect('ABCldS').expect({point: 3, start: 3, end: 23, markRing: [], length: 6});

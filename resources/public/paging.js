/*jslint node: true, stupid: true, nomen: true */
/*globals Promise */
/*jshint node: true */
/*eslint-env node */
/*eslint quotes: [2, "single"] */
/*eslint-disable no-underscore-dangle */

'use strict';

function LRU(max) {
    this.cache = [].constructor(max);
    this.index = [].constructor(max);
    this.max = max;
}

LRU.prototype.set = function (key, value) {
    var idx = this.index.indexOf(key);
    if (idx === 0) {
        this.cache[0] = value;
        return value;
    }
    if (idx > 0) {
        this.index.splice(idx, 1);
        this.cache.splice(idx, 1);
    }
    this.index.unshift(key);
    this.cache.unshift(value);
    this.index = this.index.slice(0, this.max);
    this.cache = this.cache.slice(0, this.max);
    return value;
};

LRU.prototype.get = function (key, orElse) {
    var value = this.cache[this.index.indexOf(key)];
    if (value === undefined) {
        if (orElse) {
            return this.set(key, orElse.call ? orElse(key) : orElse);
        }
    } else {
        return this.set(key, value);
    }
};

// client

function RemoteBuffer(frame, name, length, options) {
    this.frame = frame;
    this.name = name;
    this.length = length;
    this.cache = new LRU(options.cacheSize || 10);
    this.notFound = options.notFound || 'x';
    this.pageSize = options.pageSize || 8 * 1024;
    this.pages = Math.round(length / this.pageSize);
    this.pageRequests = {};
    this.missingPage = [].constructor(this.pageSize + 1).join(this.notFound);
}

RemoteBuffer.prototype.onpage = function (message) {
    this.cache.set(message.page, message.content);
    this.pageRequests[message.page].forEach(function (callback) {
        callback();
    });
    delete this.pageRequests[message.page];
};

RemoteBuffer.prototype.pageIndex = function (index) {
    return Math.floor(index / this.pageSize);
};

RemoteBuffer.prototype.pageAt = function (index, callback) {
    var pageIndex = this.pageIndex(index),
        page = this.cache.get(pageIndex),
        requests = this.pageRequests[pageIndex];
    if (!page) {
        if (!requests) {
            this.pageRequests[pageIndex] = callback ? [callback] : [];
            this.frame.send({type: 'page', name: this.name, scope: 'buffer',
                             page: pageIndex, pageSize: this.pageSize});
        } else if (callback) {
            requests.push(callback);
        }
    } else if (callback) {
        callback();
    }
    return page || this.missingPage;
};

RemoteBuffer.prototype.charAt = function (index, callback) {
    if (index < 0 || index >= this.length) {
        return '';
    }
    var that = this,
        page = this.pageAt(index, callback && function () { callback(that.charAt(index)); });
    return page[index - this.pageIndex(index) * this.pageSize];
};


RemoteBuffer.prototype.charAtAsync = function (index) {
    return new Promise(this.charAt.bind(this, index));
};

RemoteBuffer.prototype.slice = function (beginSlice, endSlice, callback) {
    beginSlice = beginSlice || 0;
    if (beginSlice < 0) {
        return '';
    }
    endSlice = endSlice === undefined ? this.length : endSlice;
    if (endSlice < 0) {
        endSlice += this.length;
    }
    endSlice = Math.min(endSlice, this.length);
    var i, s = '', that = this,
        lastPageCallback = function (index) {
            return that.pageIndex(index) === that.pageIndex(endSlice - 1) && callback && function () {
                callback(that.slice(beginSlice, endSlice));
            };
        },
        firstPageSize = this.pageSize - beginSlice % this.pageSize;
    s = this.pageAt(beginSlice, lastPageCallback(beginSlice)).slice(this.pageSize - firstPageSize);
    for (i = beginSlice + s.length; i < endSlice + this.pageSize % endSlice; i += this.pageSize) {
        s += this.pageAt(i, lastPageCallback(i));
    }
    return s.slice(0, endSlice - beginSlice);
};

RemoteBuffer.prototype.sliceAsync = function (beginSlice, endSlice) {
    return new Promise(this.slice.bind(this, beginSlice, endSlice));
};

var WebSocket = require('ws');
var Rope = require('./rope').Rope;
var RopeBuffer = require('./rope').RopeBuffer;

function BufferText(beg) {
    this.beg = beg;
}

function Buffer(remoteBuffer, text) {
    this.remoteBuffer = remoteBuffer;
    this.text = text;
}

Buffer.prototype.onpage = function (message) {
    this.remoteBuffer.onpage(message);
};

function Window() { return; }

function Frame(url, onopen, options) {
    this.ws = new WebSocket(url);
    this.onopen = onopen;
    this.options = options;
    this.ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

Frame.prototype.send = function (message, what) {
    var data = JSON.stringify(message);
    console.log('client %s %s:', message.scope, what || 'request', data);
    this.ws.send(data);
};

Frame.prototype.onerror = function (e) {
    console.log('client frame error:', this.name, e);
};

Frame.prototype.onclose = function () {
    console.log('client frame closed:', this.name);
};

Frame.prototype.onmessage = function (data) {
    var message = JSON.parse(data),
        handler = 'on' + message.type;
    console.log('client %s received:', message.scope, data);
    if (message.scope === 'frame') {
        this[handler](message);
    }
    if (message.scope === 'window') {
        this.windows[message.sequenceNumber][handler](message);
    }
    if (message.scope === 'buffer') {
        this.buffers[message.name][handler](message);
    }
};

Frame.prototype.oninit = function (message) {
    var that = this;
    this.name = message.name;
    this.buffers = Object.keys(message.buffers).reduce(function (acc, k) {
        var buffer = message.buffers[k],
            remoteBuffer = new RemoteBuffer(that, k, buffer.size, that.options);
        Object.setPrototypeOf(buffer.text, new BufferText(new RopeBuffer(remoteBuffer, 0, remoteBuffer.length)));
        acc[k] = Object.setPrototypeOf(buffer, new Buffer(remoteBuffer));
        return acc;
    }, {});
    this.onlayout(message);
    this.onopen(this);
};

Frame.prototype.onlayout = function (message) {
    var windows = {};
    function toWindow(window) {
        if (window) {
            if (windows[window.sequenceNumber]) {
                return windows[window.sequenceNumber];
            }
            windows[window.sequenceNumber] = Object.setPrototypeOf(window, new Window());
            return ['next', 'prev', 'hchild', 'vchild', 'parent'].reduce(function (w, p) {
                w[p] = toWindow(w[p]);
                return w;
            }, window);
        }
    }
    this.rootWindow = toWindow(message.rootWindow);
    this.minibufferWindow = toWindow(message.minibufferWindow);
    this.selectedWindow = toWindow(message.selectedWindow);
    this.windows = windows;
};

// server

function ServerWindow(buffer, isMini, next, prev, hchild, vchild, parent, leftCol, topLine,
                      normalLines, normalCols, start, pointm) {
    this.sequenceNumber = ServerWindow.nextSequenceNumber();
    this.buffer = buffer;
    this.isMini = isMini || false;
    this.next = next;
    this.prev = prev;
    this.hchild = hchild;
    this.vchild = vchild;
    this.parent = parent;
    this.leftCol = leftCol || 0;
    this.topLine = topLine || 0;
    this.normalLines = normalLines;
    this.normalCols = normalCols;
    this.start = start || 0;
    this.pointm = pointm || 0;
}

ServerWindow.nextSequenceNumber = (function () {
    var sequenceNumber = 0;
    return function () {
        sequenceNumber += 1;
        return sequenceNumber;
    };
}());

function ServerFrame(ws, name, editor) {
    this.ws = ws;
    this.name = name;
    this.editor = editor;
    this.rootWindow = new ServerWindow('*scratch*');
    this.minibufferWindow = new ServerWindow(' *Minibuf-0*', true);
    this.selectedWindow = this.rootWindow;
    ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

ServerFrame.prototype.onerror = function (e) {
    console.log('server frame error:', this.name, e);
};

ServerFrame.prototype.onclose = function () {
    delete this.editor.frames[this.name];
    console.log('server frame closed:', this.name);
};

ServerFrame.prototype.send = function (message, what) {
    if (message) {
        var data = JSON.stringify(message);
        console.log('server %s %s:', message.scope, what || 'reply', data);
        this.ws.send(data);
    }
};

ServerFrame.prototype.onmessage = function (data) {
    var message = JSON.parse(data),
        handler = 'on' + message.type;
    console.log('server %s received:', message.scope, data);
    if (message.scope === 'frame') {
        this.send(this[handler](message));
    }
    if (message.scope === 'buffer') {
        this.send(this.editor.buffers[message.name][handler](message));
    }
};

function ServerBufferText(beg, modiff, saveModiff, markers) {
    this.beg = beg;
    this.modiff = modiff || 0;
    this.saveModiff = saveModiff || 0;
    this.markers = markers || [];
}

ServerBufferText.prototype.insert = function (pt, args) {
    return new ServerBufferText(this.beg.insert(pt, args), this.modiff + 1,
                                this.saveModiff, this.markers);
};

ServerBufferText.prototype.deleteRegion = function (start, end) {
    return new ServerBufferText(this.beg.del(start, end), this.modiff + 1,
                                this.saveModiff, this.markers);
};

function ServerBuffer(name, text, pt, begv, zv, mark) {
    this.name = name;
    this.text = new ServerBufferText(text);
    this.pt = pt || 0;
    this.begv = begv || 0;
    this.zv = zv || this.text.beg.length;
    this.mark = mark || 0;
    this._revisions = [this.text];
    this._currentRevision = this._revisions.length - 1;
}

ServerBuffer.prototype.limitToRegion = function (position) {
    return Math.max(this.begv, Math.min(position, this.zv));
};

ServerBuffer.prototype.gotoChar = function (position) {
    var previousPt = this.pt;
    this.pt = this.limitToRegion(position);
    this.server.broadcast({type: 'gotoChar', scope: 'buffer', name: this.name, pt: this.pt, before: {pt: previousPt}});
};

ServerBuffer.prototype.newRevision = function (text) {
    this.text = text;
    this.zv = text.beg.length;
    this._revisions = this._revisions.slice(0, this._currentRevision);
    this._revisions.push(this.text);
    this._currentRevision = this._revisions.length + 1;
};

ServerBuffer.prototype.insert = function (args) {
    var previousPt = this.pt;
    this.newRevision(this.text.insert(previousPt, args));
    this.server.broadcast({type: 'insert', scope: 'buffer', name: this.name, args: args,
                           before: {pt: previousPt}, after: {modiff: this.text.modiff, zv: this.zv}});
    this.gotoChar(previousPt + args.length);
};

ServerBuffer.prototype.deleteRegion = function (start, end) {
    start = this.limitToRegion(start || this.pt);
    end = this.limitToRegion(end || this.pt);
    this.newRevision(this.text.deleteRegion(start, end));
    this.server.broadcast({type: 'deleteRegion', scope: 'buffer', name: this.name,
                           start: start, end: end,
                           after: {modiff: this.text.modiff, zv: this.zv}});
};

ServerBuffer.prototype.undo = function (arg) {
    arg = arg === undefined ? 1 : arg;
    if (arg > 0 && this._currentRevision > 0) {
        this._currentRevision = this._currentRevision - 1;
        this.text = this._revisions[this._currentRevision];
        this.zv = this.text.beg.length;
        this.server.broadcast({type: 'undo', scope: 'buffer', name: this.name,
                               after: {modiff: this.text.modiff, zv: this.zv}});
        this.undo(arg - 1);
    }
};

ServerBuffer.prototype.redo = function (arg) {
    arg = arg === undefined ? 1 : arg;
    if (arg > 0 && this._revisions.length - 1 > this._currentRevision) {
        this._currentRevision = this._currentRevision + 1;
        this.text = this._revisions[this._currentRevision];
        this.zv = this.text.beg.length;
        this.server.broadcast({type: 'redo', scope: 'buffer', name: this.name,
                               after: {modiff: this.text.modiff, zv: this.zv}});
        this.redo(arg - 1);
    }
};

ServerBuffer.prototype.onpage = function (message) {
    var pageSize = message.pageSize,
        beginSlice = message.page * pageSize,
        savedVersion = this._revisions[this.text.saveModiff];
    message.content = savedVersion.beg.slice(beginSlice, beginSlice + pageSize).toString();
    return message;
};

var WebSocketServer = require('ws').Server;

function EditorServer(buffers, options) {
    this.wss = new WebSocketServer(options);
    var that = this;
    this.buffers = buffers.reduce(function (acc, buffer) {
        acc[buffer.name] = buffer;
        buffer.server = that;
        return acc;
    }, {});
    this.url = 'ws://' + this.wss.options.host + ':' + this.wss.options.port + (this.wss.options.path || '');
    this.frames = [];
    this.wss.on('connection', this.onconnection.bind(this))
        .on('error', this.onerror.bind(this));
}

EditorServer.prototype.onconnection = function (ws) {
    var name = this.frames.length, buffers = this.buffers,
        frame = new ServerFrame(ws, name, this),
        bufferMeta = Object.keys(buffers).reduce(function (acc, k) {
            var buffer = buffers[k];
            acc[k] = {name: buffer.name, size: buffer.text.beg.length,
                      text: {modiff: buffer.text.modiff, saveModiff: buffer.text.saveModiff, markers: buffer.text.markers},
                      begv: buffer.begv, zv: buffer.zv,
                      pt: buffer.pt, mark: buffer.mark};
            return acc;
        }, {}),
        message = {type: 'init', name: name, scope: 'frame',
                   buffers: bufferMeta,
                   rootWindow: frame.rootWindow,
                   minibufferWindow: frame.minibufferWindow,
                   selectedWindow: frame.selectedWindow};
    frame.send(message, 'connection');
    this.frames[name] = frame;
};

EditorServer.prototype.onerror = function (e) {
    console.log('server error:', e);
};

EditorServer.prototype.broadcast = function (message, what) {
    var data = JSON.stringify(message);
    console.log('server %s %s:', message.scope, what || 'broadcast', data);
    this.frames.forEach(function (frame) {
        frame.ws.send(data);
    });
};

var assert = require('assert'),
    path = require('path');

var tutorial = require('fs').readFileSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'), {encoding: 'utf8'});
var scratch = ';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.\n;; If you want to create a file, visit that file with C-x C-f,\n\n;; then enter the text in that file\'s own buffer.\n';

var server = new EditorServer([new ServerBuffer('TUTORIAL', Rope.toRope(tutorial)),
                               new ServerBuffer('*scratch*', Rope.toRope(scratch)),
                               new ServerBuffer(' *Minibuf-0*', Rope.EMPTY)], {port: 8080, path: '/ws'});
var client = new Frame(server.url, function (frame) {
    var buffers = frame.buffers, TUTORIAL = buffers.TUTORIAL.remoteBuffer, error, callbacksCalled = 0;
    assert.equal(frame.name, 0);
    assert.equal(Object.keys(frame.windows).length, 2);
    assert.equal(frame.selectedWindow, frame.windows[1]);
    assert.equal(frame.rootWindow, frame.windows[1]);
    assert.deepEqual(Object.keys(buffers), ['TUTORIAL', '*scratch*', ' *Minibuf-0*']);
    assert.equal(buffers.TUTORIAL.pt, 0);
    assert.equal(TUTORIAL.charAt(-1), '');
    assert.equal(TUTORIAL.charAt(TUTORIAL.length), '');
    assert.equal(TUTORIAL.notFound, 'x');
    assert.equal(TUTORIAL.charAt(0), TUTORIAL.notFound, 'charAt page miss');
    TUTORIAL.charAt(0, function (x) {
        callbacksCalled += 1;
        assert.equal(x, tutorial.charAt(0), 'charAt callback no cache');
        assert.equal(TUTORIAL.charAt(0), 'E', 'charAtSync within cache');
        TUTORIAL.charAt(0, function (y) {
            callbacksCalled += 1;
            assert.equal(y, tutorial.charAt(0), 'charAtSync with cache using callback');
        });
        TUTORIAL.charAtAsync(10000).then(function (y) {
            callbacksCalled += 1;
            assert.equal(y, tutorial.charAt(10000), 'charAtAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    TUTORIAL.slice(0, 256, function (x) {
        callbacksCalled += 1;
        assert.equal(x, tutorial.slice(0, 256), 'slice callback, partial cache');
        assert.equal(TUTORIAL.slice(64, 128), tutorial.slice(64, 128), 'sliceSync within cache');
        assert.equal(TUTORIAL.slice(64, 128 - TUTORIAL.length), tutorial.slice(64, 128 - TUTORIAL.length), 'sliceSync within cache, negative end');
        assert.equal(TUTORIAL.slice(0, 0), '', 'sliceSync within cache empty');
        assert.equal(TUTORIAL.slice(-1, -1), '', 'sliceSync within cache both negative');
        assert.equal(TUTORIAL.slice(64, 0), '', 'sliceSync within cache begin larger than end');
        assert.equal(TUTORIAL.slice(64, -TUTORIAL.length), '', 'sliceSync within cache begin larger than negative end');
        TUTORIAL.slice(0, 128, function (y) {
            callbacksCalled += 1;
            assert.equal(y, tutorial.slice(0, 128), 'sliceSync within cache using callback');
        });
        TUTORIAL.sliceAsync(20000, 20128).then(function (y) {
            callbacksCalled += 1;
            assert.equal(y, tutorial.slice(20000, 20128), 'sliceAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    setTimeout(function () {
        server.wss.close();
        if (error) {
            throw error;
        }
        assert.equal(callbacksCalled, 6);
    }, 100);
}, {pageSize: 128});
assert(client.ws.url, server.url);

var lru = new LRU(3);

lru.set(1, 'foo');
lru.set(2, 'bar');
lru.set(3, 'baz');

assert.deepEqual(lru, {cache: ['baz', 'bar', 'foo'], index: [3, 2, 1], max: 3});
assert.equal('foo', lru.get(1));
assert.deepEqual(lru, {cache: ['foo', 'baz', 'bar'], index: [1, 3, 2], max: 3});

lru.set(4, 'quux');
assert.deepEqual(lru, {cache: ['quux', 'foo', 'baz'], index: [4, 1, 3], max: 3});

lru.set(2, 'woz');
assert.deepEqual(lru, {cache: ['woz', 'quux', 'foo'], index: [2, 4, 1], max: 3});
assert(!lru.get(5));

assert.equal('KEY 5', lru.get(5, function (k) { return 'KEY ' + k; }));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('KEY 5', lru.get(5));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('jobs', lru.set(5, 'jobs'));
assert.deepEqual(lru, {cache: ['jobs', 'woz', 'quux'], index: [5, 2, 4], max: 3});

/*jslint node: true, stupid: true, nomen: true */
/*globals Promise */
/*jshint node: true */
/*eslint-env node */
/*eslint quotes: [2, "single"] */

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

function RemoteBuffer(ws, name, length, options) {
    this.ws = ws;
    this.name = name;
    this.length = length;
    this.cache = new LRU(options['cache-size'] || 10);
    this.notFound = options['not-found'] || 'x';
    this.pageSize = options['page-size'] || 8 * 1024;
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

RemoteBuffer.prototype.request = function (message) {
    var data = JSON.stringify(message);
    console.log('client buffer request:', data);
    this.ws.send(data);
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
            this.request({type: 'page', name: this.name, scope: 'buffer',
                          page: pageIndex, 'page-size': this.pageSize});

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

function Buffer(buffer) {
    this.buffer = buffer;
    this.bufferText = new RopeBuffer(buffer, 0, buffer.length);
}

Buffer.prototype.onpage = function (message) {
    this.buffer.onpage(message);
};

function Window(buffer, isMiniBufferWindow, isLiveWindow, left, right, direction) {
    this.buffer = buffer;
    this.isMiniBufferWindow = isMiniBufferWindow;
    this.isLiveWindow = isLiveWindow;
    this.left = left;
    this.right = right;
    this.direction = direction;
}

function Frame(url, onopen, options) {
    this.ws = new WebSocket(url);
    this.onopen = onopen;
    this.options = options;
    this.ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

Frame.prototype.onerror = function (e) {
    console.log('client frame error:', this.id, e);
};

Frame.prototype.onclose = function () {
    console.log('client frame closed:', this.id);
};

Frame.prototype.onmessage = function (data) {
    var message = JSON.parse(data),
        handler = 'on' + message.type;
    console.log('client received:', data);
    if (message.scope === 'frame') {
        this[handler](message);
    }
    if (message.scope === 'buffer') {
        this.buffers[message.name][handler](message);
    }
};

Frame.prototype.oninit = function (message) {
    var that = this;
    this.id = message.id;
    this.buffers = Object.keys(message.buffers).reduce(function (acc, k) {
        acc[k] = new Buffer(new RemoteBuffer(that.ws, k, message.buffers[k].length, that.options));
        return acc;
    }, {});
    this.onlayout(message);
    this.onopen(this);
};

Frame.prototype.onlayout = function (message) {
    var that = this;
    this.windows = message.windows.map(function (w) {
        return new Window(that.buffers[w.buffer], w.isMiniBufferWindow,
                          w.isLiveWindow, w.left, w.right, w.direction);

    });
    this.selectedWindow = this.windows[message['selected-window']];
};

// server

function RemoteWindow(buffer, isMiniBufferWindow, left, right, direction) {
    this.buffer = buffer;
    this.isMiniBufferWindow = isMiniBufferWindow || false;
    this.isLiveWindow = !(left && right);
    this.left = left;
    this.right = right;
    this.direction = direction;
}

function RemoteFrame(ws, id, editor) {
    this.ws = ws;
    this.id = id;
    this.editor = editor;
    this.windows = [new RemoteWindow('*scratch*'),
                    new RemoteWindow(' *Minibuf-0*', true)];
    this.selectedWindow = 0;
    ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

RemoteFrame.prototype.onerror = function (e) {
    console.log('server frame error:', this.id, e);
};

RemoteFrame.prototype.onclose = function () {
    delete this.editor.frames[this.id];
    console.log('server frame closed:', this.id);
};

RemoteFrame.prototype.onmessage = function (data) {
    var message = JSON.parse(data);
    console.log('server received:', data);
    message = this['on' + message.type](message);
    if (message) {
        data = JSON.stringify(message);
        console.log('server reply:', data);
        this.ws.send(data);
    }
};

RemoteFrame.prototype.onpage = function (message) {
    var pageSize = message['page-size'],
        beginSlice = message.page * pageSize;
    message.content = this.editor.buffers[message.name].slice(beginSlice, beginSlice + pageSize).toString();
    return message;
};

var WebSocketServer = require('ws').Server;

function EditorServer(buffers, options) {
    this.wss = new WebSocketServer(options);
    this.buffers = buffers;
    this.url = 'ws://' + this.wss.options.host + ':' + this.wss.options.port + (this.wss.options.path || '');
    this.frames = [];
    this.wss.on('connection', this.onconnection.bind(this))
        .on('error', this.onerror.bind(this));
}

EditorServer.prototype.onconnection = function (ws) {
    var id = this.frames.length, buffers = this.buffers,
        frame = new RemoteFrame(ws, id, this),
        bufferMeta = Object.keys(buffers).reduce(function (acc, k) {
            acc[k] = {length: buffers[k].length};
            return acc;
        }, {}),
        data = JSON.stringify({type: 'init', id: id, scope: 'frame',
                               buffers: bufferMeta, windows: frame.windows,
                               'selected-window': frame.selectedWindow});
    console.log('server frame connection:', data);
    ws.send(data);
    this.frames[id] = frame;
};

EditorServer.prototype.onerror = function (e) {
    console.log('server error:', e);
};

var assert = require('assert'),
    path = require('path');

var text = require('fs').readFileSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'), {encoding: 'utf8'});
var tutorial = Rope.toRope(text);

var scratch = Rope.toRope(';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.\n;; If you want to create a file, visit that file with C-x C-f,\n\n;; then enter the text in that file\'s own buffer.\n');

var server = new EditorServer({TUTORIAL: tutorial,
                               '*scratch*': scratch,
                               ' *Minibuf-0*': Rope.EMPTY}, {port: 8080, path: '/ws'});
var client = new Frame(server.url, function (frame) {
    var buffers = frame.buffers, TUTORIAL = buffers.TUTORIAL.buffer, error, callbacksCalled = 0;
    assert.equal(frame.id, 0);
    assert.equal(frame.windows.length, 2);
    assert.equal(frame.selectedWindow, frame.windows[0]);
    assert.deepEqual(Object.keys(buffers), ['TUTORIAL', '*scratch*', ' *Minibuf-0*']);
    assert.equal(TUTORIAL.charAt(-1), '');
    assert.equal(TUTORIAL.charAt(TUTORIAL.length), '');
    assert.equal(TUTORIAL.notFound, 'x');
    assert.equal(TUTORIAL.charAt(0), TUTORIAL.notFound, 'charAt page miss');
    TUTORIAL.charAt(0, function (x) {
        callbacksCalled += 1;
        assert.equal(x, text.charAt(0), 'charAt callback no cache');
        assert.equal(TUTORIAL.charAt(0), 'E', 'charAtSync within cache');
        TUTORIAL.charAt(0, function (y) {
            callbacksCalled += 1;
            assert.equal(y, text.charAt(0), 'charAtSync with cache using callback');
        });
        TUTORIAL.charAtAsync(10000).then(function (y) {
            callbacksCalled += 1;
            assert.equal(y, text.charAt(10000), 'charAtAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    TUTORIAL.slice(0, 256, function (x) {
        callbacksCalled += 1;
        assert.equal(x, text.slice(0, 256), 'slice callback, partial cache');
        assert.equal(TUTORIAL.slice(64, 128), text.slice(64, 128), 'sliceSync within cache');
        assert.equal(TUTORIAL.slice(64, 128 - TUTORIAL.length), text.slice(64, 128 - TUTORIAL.length), 'sliceSync within cache, negative end');
        assert.equal(TUTORIAL.slice(0, 0), '', 'sliceSync within cache empty');
        assert.equal(TUTORIAL.slice(-1, -1), '', 'sliceSync within cache both negative');
        assert.equal(TUTORIAL.slice(64, 0), '', 'sliceSync within cache begin larger than end');
        assert.equal(TUTORIAL.slice(64, -TUTORIAL.length), '', 'sliceSync within cache begin larger than negative end');
        TUTORIAL.slice(0, 128, function (y) {
            callbacksCalled += 1;
            assert.equal(y, text.slice(0, 128), 'sliceSync within cache using callback');
        });
        TUTORIAL.sliceAsync(20000, 20128).then(function (y) {
            callbacksCalled += 1;
            assert.equal(y, text.slice(20000, 20128), 'sliceAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    setTimeout(function () {
        server.wss.close();
        assert.equal(callbacksCalled, 6);
        if (error) {
            throw error;
        }
    }, 100);
}, {'page-size': 128});
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

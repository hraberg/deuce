/*jslint node: true, stupid: true, nomen: true */
/*globals Promise */
/*jshint node: true */
/*eslint-env node */
/*eslint  quotes: [0, "double"] */

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

function RemoteBuffer(ws, name, length, options) {
    this.ws = ws;
    this.name = name;
    this.length = length;
    this.cache = new LRU(options['cache-size'] || 10);
    this.notFound = options['not-found'] || 'x';
    this.pageSize = options['page-size'] || 8 * 1024;
    this.pages = Math.round(length / this.pageSize);
    this.missingPage = [].constructor(this.pageSize + 1).join(this.notFound);
    this.requestedPages = {};
    this.callbacks = {};
    this.lastRequestId = 0;
}

RemoteBuffer.prototype.handle = function (message) {
    var requestId = message['request-id'];
    this['on' + message.type](message);
    if (this.callbacks[requestId]) {
        this.callbacks[requestId]();
        delete this.callbacks[requestId];
    }
};

RemoteBuffer.prototype.onpage = function (message) {
    this.cache.set(message.page, message.content);
    delete this.requestedPages[message.page];
};

RemoteBuffer.prototype.nextRequestId = function () {
    this.lastRequestId += 1;
    return this.lastRequestId;
};

RemoteBuffer.prototype.request = function (message, callback) {
    var requestId = this.nextRequestId(),
        data;
    message['request-id'] = requestId;
    data = JSON.stringify(message);
    console.log('client buffer request:', data);
    if (callback) {
        this.callbacks[requestId] = callback;
    }
    this.ws.send(data);
};

RemoteBuffer.prototype.pageIndex = function (index) {
    return Math.floor(index / this.pageSize);
};

RemoteBuffer.prototype.pageAt = function (index, callback) {
    var pageIndex = this.pageIndex(index),
        page = this.cache.get(pageIndex);
    if (!page && !this.requestedPages[pageIndex]) {
        this.requestedPages[pageIndex] = true;
        this.request({type: 'page', name: this.name, scope: 'buffer',
                      page: pageIndex, 'page-size': this.pageSize},
                     callback);
    } else if (page && callback) {
        callback();
    }
    return page || this.missingPage;
};

RemoteBuffer.prototype.charAt = function (index, callback) {
    if (index < 0 || index >= this.length) {
        return '';
    }
    var that = this,
        pageIndex = this.pageIndex(index),
        page = this.pageAt(index, callback && function () { callback(that.charAt(index)); });
    return page[index - pageIndex * this.pageSize];
};


RemoteBuffer.prototype.charAtAsync = function (index) {
    var that = this;
    return new Promise(function (resolve) {
        that.charAt(index, resolve);
    });
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
    var that = this;
    return new Promise(function (resolve) {
        that.slice(beginSlice, endSlice, resolve);
    });
};

var WebSocket = require('ws');

function EditorClientFrame(url, onopen, options) {
    this.ws = new WebSocket(url);
    this.onopen = onopen;
    this.options = options;
    this.ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

EditorClientFrame.prototype.onerror = function (e) {
    console.log('client frame error:', this.id, e);
};

EditorClientFrame.prototype.onclose = function () {
    console.log('client frame closed:', this.id);
};

EditorClientFrame.prototype.onmessage = function (data) {
    var message = JSON.parse(data);
    console.log('client received:', data);
    if (message.scope === 'frame') {
        this['on' + message.type](message);
    }
    if (message.scope === 'buffer') {
        this.buffers[message.name].handle(message);
    }
};

EditorClientFrame.prototype.oninit = function (message) {
    var that = this;
    this.id = message.id;
    this.buffers = Object.keys(message.buffers).reduce(function (acc, k) {
        acc[k] = new RemoteBuffer(that.ws, k, message.buffers[k].length, that.options);
        return acc;
    }, {});
    this.onopen(this);
};

function RemoteFrame(ws, id, editor) {
    this.ws = ws;
    this.id = id;
    this.editor = editor;
    this.windows = [];
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
    if (message['request-id']) {
        data = JSON.stringify(message);
        console.log('server reply:', data);
        this.ws.send(data);
    }
};

RemoteFrame.prototype.onpage = function (message) {
    var pageSize = message['page-size'],
        beginSlice = message.page * pageSize;
    message.content = this.editor.buffers[message.name].slice(beginSlice, beginSlice + pageSize);
    return message;
};

var WebSocketServer = require('ws').Server;

function EditorServer(port, buffers) {
    this.wss = new WebSocketServer({port: port});
    this.buffers = buffers;
    this.url = 'ws://' + this.wss.options.host + ':' + this.wss.options.port + '/' + this.wss.options.path;
    this.frames = [];
    this.wss.on('connection', this.onconnection.bind(this))
        .on('error', this.onerror.bind(this));
}

EditorServer.prototype.onconnection = function (ws) {
    var id = this.frames.length, buffers = this.buffers,
        bufferMeta = Object.keys(buffers).reduce(function (acc, k) {
            acc[k] = {length: buffers[k].length};
            return acc;
        }, {}),
        data = JSON.stringify({type: 'init', id: id, scope: 'frame', buffers: bufferMeta});
    console.log('server frame connection:', data);
    ws.send(data);
    this.frames[id] = new RemoteFrame(ws, id, this);
};

EditorServer.prototype.onerror = function (e) {
    console.log('server error:', e);
};

var assert = require('assert'),
    path = require('path');

var text = require('fs').readFileSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'), {encoding: 'utf8'});
var server = new EditorServer(8080, {TUTORIAL: text});
var client = new EditorClientFrame(server.url, function (frame) {
    var buffers = frame.buffers, error;
    assert.equal(frame.id, 0);
    assert.deepEqual(Object.keys(buffers), ['TUTORIAL']);
    assert.equal(buffers.TUTORIAL.charAt(-1), '');
    assert.equal(buffers.TUTORIAL.charAt(buffers.TUTORIAL.length), '');
    assert.equal(buffers.TUTORIAL.notFound, 'x');
    assert.equal(buffers.TUTORIAL.charAt(0), buffers.TUTORIAL.notFound, 'charAt page miss');
    buffers.TUTORIAL.charAt(0, function (x) {
        assert.equal(x, text.charAt(0), 'charAt callback no cache');
        assert.equal(buffers.TUTORIAL.charAt(0), 'E', 'charAtSync within cache');
        buffers.TUTORIAL.charAt(0, function (y) {
            assert.equal(y, text.charAt(0), 'charAtSync with cache using callback');
        });
        buffers.TUTORIAL.charAtAsync(10000).then(function (y) {
            assert.equal(y, text.charAt(10000), 'charAtAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    buffers.TUTORIAL.slice(0, 256, function (x) {
        assert.equal(x, text.slice(0, 256), 'slice callback, partial cache');
        assert.equal(buffers.TUTORIAL.slice(64, 128), text.slice(64, 128), 'sliceSync within cache');
        assert.equal(buffers.TUTORIAL.slice(64, 128 - buffers.TUTORIAL.length), text.slice(64, 128 - buffers.TUTORIAL.length), 'sliceSync within cache, negative end');
        assert.equal(buffers.TUTORIAL.slice(0, 0), '', 'sliceSync within cache empty');
        assert.equal(buffers.TUTORIAL.slice(-1, -1), '', 'sliceSync within cache both negative');
        assert.equal(buffers.TUTORIAL.slice(64, 0), '', 'sliceSync within cache begin larger than end');
        assert.equal(buffers.TUTORIAL.slice(64, -buffers.TUTORIAL.length), '', 'sliceSync within cache begin larger than negative end');
        buffers.TUTORIAL.slice(0, 128, function (y) {
            assert.equal(y, text.slice(0, 128), 'sliceSync within cache using callback');
        });
        buffers.TUTORIAL.sliceAsync(20000, 20128).then(function (y) {
            assert.equal(y, text.slice(20000, 20128), 'sliceAsync no cache');
        }).catch(function (e) {
            error = e;
        });
    });
    setTimeout(function () {
        server.wss.close();
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

assert.equal('KEY 5', lru.get(5, function (k) { return "KEY " + k; }));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('KEY 5', lru.get(5));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('jobs', lru.set(5, 'jobs'));
assert.deepEqual(lru, {cache: ['jobs', 'woz', 'quux'], index: [5, 2, 4], max: 3});

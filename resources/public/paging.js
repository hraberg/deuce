/*jslint node: true stupid: true nomen: true */

'use strict';

function LRU(max) {
    this.cache = [].constructor(max);
    this.index = [].constructor(max);
    this.max = max;
}

LRU.prototype.set = function (key, value) {
    var idx = this.index.indexOf(key);
    if (idx >= 0) {
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

function RemoteBuffer(ws, id, length, pageSize, notFound, cacheSize) {
    this.ws = ws;
    this.id = id;
    this.length = length;
    this.pageSize = pageSize || 8 * 1024;
    this.pages = Math.round(length / this.pageSize);
    this.cache = new LRU(cacheSize || 10);
    this.requestedPages = {};
    this.notFound = notFound || 'x';
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
    delete this.requestedPages[message.data.page];
    this.cache.set(message.data.page, message.data.content);
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

RemoteBuffer.prototype.charAt = function (index, callback) {
    if (index < 0 || index >= this.length) {
        return '';
    }
    var pageIndex = this.pageIndex(index),
        page = this.cache.get(pageIndex),
        that = this;
    if (!page && !this.requestedPages[pageIndex]) {
        this.requestedPages[pageIndex] = true;
        this.request({type: 'page', id: this.id, scope: 'buffer',
                      data: {page: pageIndex, 'page-size': this.pageSize}},
                     function () { callback(that.charAt(index)); });
    }
    return (page && page[index - pageIndex * this.pageSize]) || this.notFound;
};

RemoteBuffer.prototype.slice = function (beginSlice, endSlice, callback) {
    beginSlice = beginSlice || 0;
    endSlice = endSlice || this.length;
    var i, s = '', that = this, called = false,
        cb = callback && function () {
            if (!called && that.cache.get(that.pageIndex(endSlice - 1))) {
                callback(that.slice(beginSlice, endSlice));
                called = true;
            }
        };
    for (i = beginSlice; i < endSlice; i += 1) {
        s += this.charAt(i, cb);
    }
    return s;
};

function EditorServer(wss, buffers) {
    this.buffers = buffers;
    this.wss = wss;
    this.url = 'ws://' + wss.options.host + ':' + wss.options.port;
    this.frames = [];
    var that = this;
    wss.on('connection', function (ws) {
        var id = that.frames.length,
            bufferMeta = Object.keys(buffers).reduce(function (acc, k) {
                acc[k] = {length: buffers[k].length};
                return acc;
            }, {}),
            data = JSON.stringify({type: 'init', id: id, scope: 'frame',
                                   data: {id: id, buffers: bufferMeta}});
        that.frames.push(ws);
        console.log('server frame connection:', data);
        ws.send(data);
        ws.on('message', function (data) {
            var message = JSON.parse(data);
            console.log('server received:', data);
            message = that['on' + message.type](message);
            if (message['request-id']) {
                data = JSON.stringify(message);
                console.log('server reply:', data);
                ws.send(data);
            }
        }).on('close', function () {
            console.log('server frame close:', id);
            that.frames.splice(id, 1);
        });
    }).on('error', function (e) {
        console.log('server error:', e);
    });
}

EditorServer.prototype.onpage = function (message) {
    var pageSize = message.data['page-size'],
        beginSlice = message.data.page * pageSize;
    message.data.content = this.buffers[message.id].slice(beginSlice, beginSlice + pageSize);
    return message;
};

EditorServer.open = function (port, buffers) {
    var WebSocketServer = require('ws').Server,
        wss = new WebSocketServer({ port: port});
    return new EditorServer(wss, buffers);
};

function EditorClientFrame(ws, onopen, pageSize) {
    this.ws = ws;
    this.onopen = onopen;
    this.pageSize = pageSize;
    var that = this;
    ws.on('message', function (data) {
        var message = JSON.parse(data);
        console.log('client received:', data);
        if (message.scope === 'frame') {
            that['on' + message.type](message);
        }
        if (message.scope === 'buffer') {
            that.buffers[message.id].handle(message);
        }
    }).on('close', function () {
        console.log('frame closed:', that.id);
    }).on('error', function (e) {
        console.log('frame error:', e);
    });
}

EditorClientFrame.prototype.oninit = function (message) {
    var that = this;
    this.id = message.id;
    this.buffers = Object.keys(message.data.buffers).reduce(function (acc, k) {
        acc[k] = new RemoteBuffer(that.ws, k, message.data.buffers[k].length, that.pageSize);
        return acc;
    }, {});
    this.onopen(this);
};

EditorClientFrame.connect = function (url, onopen, pageSize) {
    var WebSocket = require('ws'),
        ws = new WebSocket(url);
    return new EditorClientFrame(ws, onopen, pageSize);
};

var text = require('fs').readFileSync(__dirname + '/../etc/tutorials/TUTORIAL', {encoding: 'utf8'});
var server = EditorServer.open(8080, {TUTORIAL: text});
EditorClientFrame.connect(server.url, function (frame) {
    var buffers = frame.buffers;
    console.log('-------');
    console.log('frame:', frame.id);
    console.log('-------');
    console.log('remote buffers:', Object.keys(buffers));
    console.log('-------');
    console.log('charAtSync no cache:', buffers.TUTORIAL.charAt(0, function (x) {
        console.log('-------');
        console.log('charAt:', x);
        console.log('-------');
        console.log('charAtSync with cache', buffers.TUTORIAL.charAt(0));
        console.log('-------');
    }));
    console.log('-------');
    console.log('sliceSync no cache:', buffers.TUTORIAL.slice(0, 256, function (x) {
        console.log('-------');
        console.log('slice:', x);
        console.log('-------');
        console.log('sliceSync within cache:', buffers.TUTORIAL.slice(64, 128));
        console.log('-------');
        server.wss.close();
    }));
}, 128);

var lru = new LRU(3);
lru.set(1, 'foo');
lru.set(2, 'bar');
lru.set(3, 'baz');

console.log(JSON.stringify(lru));
console.log(lru.get(1));
console.log(JSON.stringify(lru));

lru.set(4, 'quux');
console.log(JSON.stringify(lru));

lru.set(2, 'woz');
console.log(JSON.stringify(lru));

console.log(lru.get(5));

console.log(lru.get(5, function (k) { return "KEY " + k; }));
console.log(JSON.stringify(lru));

console.log(lru.get(5));
console.log(JSON.stringify(lru));

console.log(lru.set(5, 'jobs'));
console.log(JSON.stringify(lru));

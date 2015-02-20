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
    this.callbacks = [];
    var that = this;
    this.ws.on('message', function (data) {
        var message = JSON.parse(data);
        if (message.scope === 'buffer') {
            console.log('client buffer received:', data);
            if (message.type === 'page' && message.id === id) {
                delete that.requestedPages[message.data.page];
                that.cache.set(message.data.page, message.data.content);
                if (that.callbacks.length > 0) {
                    that.callbacks.shift()();
                }
            }
        }
    });
}

RemoteBuffer.prototype.pageIndex = function (index) {
    return Math.floor(index / this.pageSize);
};

RemoteBuffer.prototype.charAt = function (index, callback) {
    if (index < 0 || index >= this.length) {
        return '';
    }
    var pageIndex = this.pageIndex(index),
        page = this.cache.get(pageIndex),
        data,
        that = this;
    if (!page && !this.requestedPages[pageIndex]) {
        data = JSON.stringify({type: 'page', id: this.id, scope: 'buffer',
                               data: {page: pageIndex, 'page-size': this.pageSize}});
        this.requestedPages[pageIndex] = true;
        console.log('client buffer request:', data);
        this.ws.send(data);
        if (callback) {
            this.callbacks.push(function () {
                callback(that.charAt(index));
            });
        }
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
    wss.on('connection', function connection(ws) {
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
            var beginSlice, pageSize, message = JSON.parse(data);
            console.log('server received:', data);
            if (message.type === 'page') {
                pageSize = message.data['page-size'];
                beginSlice = message.data.page * pageSize;
                message.data.content = buffers[message.id].slice(beginSlice, beginSlice + pageSize);
                data = JSON.stringify(message);
                console.log('server reply:', data);
                ws.send(data);
            }
        });
        ws.on('close', function () {
            console.log('server frame close:', id);
            that.frames.splice(id, 1);
        });
    });
    wss.on('error', function (e) {
        console.log(e);
    });
}

EditorServer.open = function (port, buffers) {
    var WebSocketServer = require('ws').Server,
        wss = new WebSocketServer({ port: port});
    return new EditorServer(wss, buffers);
};

function EditorClientFrame(ws, id, buffers) {
    this.ws = ws;
    this.id = id;
    this.buffers = buffers;
    ws.on('close', function () {
        console.log('frame closed:', id);
    });
}

EditorClientFrame.connect = function (url, callback, pageSize) {
    var WebSocket = require('ws'),
        ws = new WebSocket(url);
    ws.on('message', function (data) {
        var message = JSON.parse(data), buffers;
        if (message.scope === 'frame') {
            console.log('client frame received:', data);
            if (message.type === 'init') {
                buffers = Object.keys(message.data.buffers).reduce(function (acc, k) {
                    acc[k] = new RemoteBuffer(ws, k, message.data.buffers[k].length, pageSize);
                    return acc;
                }, {});
                callback(new EditorClientFrame(ws, message.data.id, buffers));
            }
        }
    });
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

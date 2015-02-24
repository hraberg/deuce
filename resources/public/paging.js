/*jshint node: true, esnext: true  */

'use strict';

function LRU(max) {
    this.cache = [].constructor(max);
    this.index = [].constructor(max);
    this.max = max;
}

LRU.prototype.set = (key, value) => {
    let idx = this.index.indexOf(key);
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

LRU.prototype.get = (key, orElse) => {
    let value = this.cache[this.index.indexOf(key)];
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

RemoteBuffer.prototype.onpage = (message) => {
    this.cache.set(message.page, message.content);
    this.pageRequests[message.page].forEach(callback => callback());
    delete this.pageRequests[message.page];
};

RemoteBuffer.prototype.pageIndex = (index) =>
    Math.floor(index / this.pageSize);

RemoteBuffer.prototype.pageAt = (index, callback) => {
    let pageIndex = this.pageIndex(index),
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

RemoteBuffer.prototype.charAt = (index, callback) => {
    if (index < 0 || index >= this.length) {
        return '';
    }
    let that = this,
        recall = () => callback(that.charAt(index)),
        page = this.pageAt(index, callback && recall);
    return page[index - this.pageIndex(index) * this.pageSize];
};


RemoteBuffer.prototype.charAtAsync = (index) =>
    new Promise(this.charAt.bind(this, index));

RemoteBuffer.prototype.slice = (beginSlice, endSlice, callback) => {
    beginSlice = beginSlice || 0;
    if (beginSlice < 0) {
        return '';
    }
    endSlice = endSlice === undefined ? this.length : endSlice;
    if (endSlice < 0) {
        endSlice += this.length;
    }
    endSlice = Math.min(endSlice, this.length);
    let s = '', that = this,
        recall = () => callback(that.slice(beginSlice, endSlice)),
        lastPageCallback = (index) => that.pageIndex(index) === that.pageIndex(endSlice - 1) && callback && recall,
        firstPageSize = this.pageSize - beginSlice % this.pageSize;
    s = this.pageAt(beginSlice, lastPageCallback(beginSlice)).slice(this.pageSize - firstPageSize);
    for (let i = beginSlice + s.length; i < endSlice + this.pageSize % endSlice; i += this.pageSize) {
        s += this.pageAt(i, lastPageCallback(i));
    }
    return s.slice(0, endSlice - beginSlice);
};

RemoteBuffer.prototype.sliceAsync = (beginSlice, endSlice) =>
    new Promise(this.slice.bind(this, beginSlice, endSlice));

const WebSocket = require('ws'),
      Rope = require('./rope').Rope,
      RopeBuffer = require('./rope').RopeBuffer;

function BufferText(beg, modiff, saveModiff, markers) {
    this.beg = beg;
    this.modiff = modiff;
    this.saveModiff = saveModiff;
    this.markers = markers;
}

BufferText.prototype.nextModificationEvent = (beg) =>
    new BufferText(beg, this.modiff + 1, this.saveModiff, this.markers);

BufferText.prototype.insert = (pt, args) =>
    this.nextModificationEvent(this.beg.insert(pt - 1, args));

BufferText.prototype.deleteRegion = (start, end) =>
    this.nextModificationEvent(this.beg.del(start - 1, end - 1));


function Buffer(remoteBuffer, text) {
    this.remoteBuffer = remoteBuffer;
    this.newRevision(text);
}

Buffer.prototype.checkConditions = (message, what) => {
    let that = this, conditions = message[what] || {};
    Object.keys(conditions).forEach(k => {
        if (conditions[k] !== that[k]) {
            throw new Error(what + '-condition not met: ' + k + ' was ' + that[k] + ' expected ' + conditions[k]);
        }
    });
};

Buffer.prototype.newRevision = (text) => {
    this.text = text;
    this.size = text.beg.length;
    this._revisions = (this._revisions || []).slice(0, this._currentRevision);
    this._revisions.push(this.text);
    this._currentRevision = this._revisions.length - 1;
};

Buffer.prototype.ongotoChar = (message) =>
    this.pt = message.position;

Buffer.prototype.onnarrowToRegion = (message) => {
    this.begv = message.begv;
    this.zv = message.zv;
};

Buffer.prototype.oninsert = (message) =>
    this.newRevision(this.text.insert(this.pt, message.args));

Buffer.prototype.ondeleteRegion = (message) =>
    this.newRevision(this.text.deleteRegion(message.start, message.end));

Buffer.prototype.onundo = () => {
    this._currentRevision = this._currentRevision - 1;
    this.text = this._revisions[this._currentRevision];
    this.size = this.text.beg.length;
};

Buffer.prototype.onpage = (message) =>
    this.remoteBuffer.onpage(message);

function Window() { return; }

function Frame(ws, onopen, options) {
    this.ws = ws;
    this.onopen = onopen;
    this.options = options;
    this.ws.on('message', this.onmessage.bind(this))
        .on('close', this.onclose.bind(this))
        .on('error', this.onerror.bind(this));
}

Frame.prototype.send = (message, what) => {
    let data = JSON.stringify(message);
    console.log('client %s %s:', message.scope, what || 'request', data);
    this.ws.send(data);
};

Frame.prototype.onerror = (e) =>
    console.log('client frame error:', this.name, e);

Frame.prototype.onclose = () =>
    console.log('client frame closed:', this.name);

Frame.prototype.onmessage = (data) => {
    let message = JSON.parse(data),
        handler = 'on' + message.type;
    console.log('client %s received:', message.scope, data);
    if (message.scope === 'frame') {
        this[handler](message);
    }
    if (message.scope === 'window') {
        this.windows[message.sequenceNumber][handler](message);
    }
    if (message.scope === 'buffer') {
        let buffer = this.buffers[message.name];
        buffer.checkConditions(message, 'pre');
        buffer[handler](message);
        buffer.checkConditions(message, 'post');
    }
};

Frame.prototype.oninit = (message) => {
    let that = this;
    this.name = message.name;
    this.buffers = Object.keys(message.buffers).reduce((acc, k) => {
        let buffer = message.buffers[k],
            remoteBuffer = new RemoteBuffer(that, k, buffer.size, that.options);
        Object.setPrototypeOf(buffer.text, new BufferText(new RopeBuffer(remoteBuffer, 0, remoteBuffer.length)));
        acc[k] = Object.setPrototypeOf(buffer, new Buffer(remoteBuffer, buffer.text));
        return acc;
    }, {});
    this.onlayout(message);
    this.onopen(this);
};

Frame.prototype.onlayout = (message) => {
    let windows = {},
        toWindow = (window) => {
            if (window) {
                if (windows[window.sequenceNumber]) {
                    return windows[window.sequenceNumber];
                }
                windows[window.sequenceNumber] = Object.setPrototypeOf(window, new Window());
                return ['next', 'prev', 'hchild', 'vchild', 'parent'].reduce((w, p) => {
                    w[p] = toWindow(w[p]);
                    return w;
                }, window);
            }
        };
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

ServerWindow.nextSequenceNumber = (() => {
    let sequenceNumber = 0;
    return () => {
        sequenceNumber += 1;
        return sequenceNumber;
    };
})();

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

ServerFrame.prototype.onerror = (e) =>
    console.log('server frame error:', this.name, e);


ServerFrame.prototype.onclose = () => {
    delete this.editor.frames[this.name];
    console.log('server frame closed:', this.name);
};

ServerFrame.prototype.send = (message, what) => {
    if (message) {
        let data = JSON.stringify(message);
        console.log('server %s %s:', message.scope, what || 'reply', data);
        this.ws.send(data);
    }
};

ServerFrame.prototype.onmessage = (data) => {
    let message = JSON.parse(data),
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

ServerBufferText.prototype.nextModificationEvent = (beg) =>
    new ServerBufferText(beg, this.modiff + 1, this.saveModiff, this.markers);


ServerBufferText.prototype.insert = (pt, args) =>
    this.nextModificationEvent(this.beg.insert(pt - 1, args));

ServerBufferText.prototype.deleteRegion = (start, end) =>
    this.nextModificationEvent(this.beg.del(start - 1, end - 1));


function ServerBuffer(name, text, pt, begv, zv, mark) {
    this.name = name;
    this.begv = begv || null;
    this.zv = zv || null;
    this.pt = pt || 1;
    this.mark = mark || null;
    this.newRevision(this.pt, new ServerBufferText(text));
}

ServerBuffer.prototype.limitToRegion = (position) =>
    Math.max(this.begv || 1, Math.min(position, this.zv || this.size));

ServerBuffer.prototype.newRevision = (pt, text) => {
    this.text = text;
    this.size = this.text.beg.length;
    this._revisions = (this._revisions || []).slice(0, this._currentRevision);
    this._revisions.push({text: this.text, pt: pt});
    this._currentRevision = this._revisions.length - 1;
};

ServerBuffer.prototype.narrowToRegion = (start, end) => {
    let previousBegv = this.begv, previousZv = this.zv;
    if (start && end && end < start) {
        let tmp = start;
        start = end;
        end = tmp;
    }
    this.begv = start ? this.limitToRegion(start) : null;
    this.zv = end ? this.limitToRegion(end) : null;
    this.server.broadcast({type: 'narrowToRegion', scope: 'buffer', name: this.name,
                           pre: {begv: previousBegv, zv: previousZv},
                           start: this.begv, end: this.zv,
                           post: {begv: this.begv, zv: this.zv}});
};

ServerBuffer.prototype.widen = () =>
    this.narrowToRegion(null, null);


ServerBuffer.prototype.lookingAt = (regexp) =>
    this.text.beg.charAt(this.pt - 1).match(regexp) || this.text.beg.slice(this.pt - 1).match(regexp);

ServerBuffer.prototype.gotoChar = (position) => {
    let previousPt = this.pt;
    this.pt = this.limitToRegion(position);
    this.server.broadcast({type: 'gotoChar', scope: 'buffer', name: this.name,
                           pre: {pt: previousPt},
                           position: this.pt,
                           post: {pt: this.pt}});
    return this.pt;
};

ServerBuffer.prototype.forwardChar = (n) =>
    this.gotoChar(this.pt + n);

ServerBuffer.prototype.backwardChar = (n) =>
    this.gotoChar(this.pt - n);

ServerBuffer.prototype.insert = (args) => {
    let previousPt = this.pt, nextPt = this.limitToRegion(previousPt + args.length);
    this.newRevision(nextPt, this.text.insert(previousPt, args));
    this.server.broadcast({type: 'insert', scope: 'buffer', name: this.name,
                           pre: {pt: previousPt},
                           args: args,
                           post: {_currentRevision: this._currentRevision, size: this.size}});
    this.gotoChar(nextPt);
};

ServerBuffer.prototype.deleteRegion = (start, end) => {
    start = this.limitToRegion(start || this.pt);
    end = this.limitToRegion(end || this.mark || this.pt);
    if (end < start) {
        let tmp = end;
        end = start;
        start = tmp;
    }
    this.newRevision(this.pt, this.text.deleteRegion(start, end));
    this.server.broadcast({type: 'deleteRegion', scope: 'buffer', name: this.name,
                           start: start, end: end,
                           post: {_currentRevision: this._currentRevision, size: this.size}});
};

ServerBuffer.prototype.undo = (arg) => {
    arg = arg === undefined ? 1 : arg;
    if (arg > 0 && this._currentRevision > 0) {
        this._currentRevision = this._currentRevision - 1;
        this.text = this._revisions[this._currentRevision].text;
        this.size = this.text.beg.length;
        this.server.broadcast({type: 'undo', scope: 'buffer', name: this.name,
                               post: {_currentRevision: this._currentRevision, size: this.size}});
        this.gotoChar(this._revisions[this._currentRevision].pt);
        this.undo(arg - 1);
    }
};

ServerBuffer.prototype.onpage = (message) => {
    let pageSize = message.pageSize,
        beginSlice = message.page * pageSize,
        savedVersion = this._revisions[this.text.saveModiff].text;
    message.content = savedVersion.beg.slice(beginSlice, beginSlice + pageSize).toString();
    return message;
};

const WebSocketServer = require('ws').Server;

function EditorServer(wss, buffers) {
    this.wss = wss;
    let that = this;
    this.buffers = buffers.reduce((acc, buffer) => {
        acc[buffer.name] = buffer;
        buffer.server = that;
        return acc;
    }, {});
    this.url = 'ws://' + this.wss.options.host + ':' + this.wss.options.port + (this.wss.options.path || '');
    this.frames = [];
    this.wss.on('connection', this.onconnection.bind(this))
        .on('error', this.onerror.bind(this));
}

EditorServer.prototype.onconnection = (ws) => {
    let name = this.frames.length, buffers = this.buffers,
        frame = new ServerFrame(ws, name, this),
        bufferMeta = Object.keys(buffers).reduce((acc, k) => {
            let buffer = buffers[k];
            acc[k] = {name: buffer.name, size: buffer.size,
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

EditorServer.prototype.onerror = (e) =>
    console.log('server error:', e);


EditorServer.prototype.broadcast = (message, what) => {
    let data = JSON.stringify(message);
    console.log('server %s %s:', message.scope, what || 'broadcast', data);
    this.frames.forEach(frame => frame.ws.send(data));
};

const assert = require('assert'),
      path = require('path');

let tutorial = require('fs').readFileSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'), {encoding: 'utf8'}),
    scratch = ';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.\n;; If you want to create a file, visit that file with C-x C-f,\n\n;; then enter the text in that file\'s own buffer.\n';

let server = new EditorServer(new WebSocketServer({port: 8080, path: '/ws'}),
                              [new ServerBuffer('TUTORIAL', Rope.toRope(tutorial)),
                               new ServerBuffer('*scratch*', Rope.toRope(scratch)),
                               new ServerBuffer(' *Minibuf-0*', Rope.EMPTY)]);
let client = new Frame(new WebSocket(server.url), frame => {
    let buffers = frame.buffers, TUTORIAL = buffers.TUTORIAL.remoteBuffer, error, callbacksCalled = 0;
    assert.equal(frame.name, 0);
    assert.equal(Object.keys(frame.windows).length, 2);
    assert.equal(frame.selectedWindow, frame.windows[1]);
    assert.equal(frame.rootWindow, frame.windows[1]);
    assert.deepEqual(Object.keys(buffers), ['TUTORIAL', '*scratch*', ' *Minibuf-0*']);
    assert.equal(buffers.TUTORIAL.pt, 1);
    assert.equal(buffers.TUTORIAL.mark, null);
    assert.equal(buffers.TUTORIAL.begv, null);
    assert.equal(buffers.TUTORIAL.zv, null);
    assert.equal(buffers.TUTORIAL.size, tutorial.length);
    assert.equal(buffers.TUTORIAL._currentRevision, 0);
    assert.equal(TUTORIAL.charAt(-1), '');
    assert.equal(TUTORIAL.charAt(TUTORIAL.length), '');
    assert.equal(TUTORIAL.notFound, 'x');
    assert.equal(TUTORIAL.charAt(0), TUTORIAL.notFound, 'charAt page miss');
    TUTORIAL.charAt(0, x => {
        callbacksCalled += 1;
        assert.equal(x, tutorial.charAt(0), 'charAt callback no cache');
        assert.equal(TUTORIAL.charAt(0), 'E', 'charAtSync within cache');
        TUTORIAL.charAt(0, y => {
            callbacksCalled += 1;
            assert.equal(y, tutorial.charAt(0), 'charAtSync with cache using callback');
        });
        TUTORIAL.charAtAsync(10000).then(y => {
            callbacksCalled += 1;
            assert.equal(y, tutorial.charAt(10000), 'charAtAsync no cache');
        }).catch(e => error = e);
    });
    TUTORIAL.slice(0, 256, x => {
        callbacksCalled += 1;
        assert.equal(x, tutorial.slice(0, 256), 'slice callback, partial cache');
        assert.equal(TUTORIAL.slice(64, 128), tutorial.slice(64, 128), 'sliceSync within cache');
        assert.equal(TUTORIAL.slice(64, 128 - TUTORIAL.length), tutorial.slice(64, 128 - TUTORIAL.length), 'sliceSync within cache, negative end');
        assert.equal(TUTORIAL.slice(0, 0), '', 'sliceSync within cache empty');
        assert.equal(TUTORIAL.slice(-1, -1), '', 'sliceSync within cache both negative');
        assert.equal(TUTORIAL.slice(64, 0), '', 'sliceSync within cache begin larger than end');
        assert.equal(TUTORIAL.slice(64, -TUTORIAL.length), '', 'sliceSync within cache begin larger than negative end');
        TUTORIAL.slice(0, 128, y => {
            callbacksCalled += 1;
            assert.equal(y, tutorial.slice(0, 128), 'sliceSync within cache using callback');
        });
        TUTORIAL.sliceAsync(20000, 20128).then(y => {
            callbacksCalled += 1;
            assert.equal(y, tutorial.slice(20000, 20128), 'sliceAsync no cache');
        }).catch(e => error = e);
    });
    setTimeout(() => {
        server.wss.close();
        if (error) {
            throw error;
        }
        assert.equal(callbacksCalled, 6);
    }, 100);
}, {pageSize: 128});
assert(client.ws.url, server.url);

let lru = new LRU(3);

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

assert.equal('KEY 5', lru.get(5, k => 'KEY ' + k));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('KEY 5', lru.get(5));
assert.deepEqual(lru, {cache: ['KEY 5', 'woz', 'quux'], index: [5, 2, 4], max: 3});

assert.equal('jobs', lru.set(5, 'jobs'));
assert.deepEqual(lru, {cache: ['jobs', 'woz', 'quux'], index: [5, 2, 4], max: 3});

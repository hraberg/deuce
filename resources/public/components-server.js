'use strict';

const diff = require('diff'),
      ws = require('ws'),
      fs = require('fs'),
      path = require('path'),
      Rope = require('./rope').Rope;

// These fields are based on Emacs/Deuce, we might not need them all.
function Window(buffer, isMini, next, prev, hchild, vchild, parent, leftCol, topLine,
                totallLines, totalCols, normalLines, normalCols, start, pointm) {
    this.sequenceNumber = Window.nextSequenceNumber();
    this.buffer = buffer;
    this.isMini = isMini || false;
    this.next = next;
    this.prev = prev;
    this.hchild = hchild;
    this.vchild = vchild;
    this.parent = parent;
    this.leftCol = leftCol || 0;
    this.topLine = topLine || 0;
    this.totalLines = totallLines;
    this.totalCols = totalCols;
    this.normalLines = normalLines;
    this.normalCols = normalCols;
    this.start = start || 1;
    this.pointm = pointm || 1;
}

Window.nextSequenceNumber = (() => {
    let sequenceNumber = 0;
    return () => {
        sequenceNumber += 1;
        return sequenceNumber;
    };
})();

Window.prototype.toViewModel = (frame) => {
    return {'sequence-number': this.sequenceNumber,
            'mini-p': this.isMini,
            'live-p': this.buffer !== undefined,
            'selected': this === frame.selectedWindow,
            'buffer': this.buffer ? this.buffer.toViewModel(frame, this) : undefined,
            'line-number-at-start': this.buffer ? this.buffer.lineNumberAtPos(this.start) : undefined,
            'mode-line': (this.isMini || !this.buffer) ? undefined : this.formatModeLine(frame)};
};

// Fake, doesn't attempt to take the buffer's mode-line-format into account.
Window.prototype.formatModeLine = (frame) => {
    let humanize = (s) => s.split('-').map((s) => s[0].toUpperCase() + s.slice(1)).join(' ');
    return '-UUU:----' + frame.name +
        '  <strong style=\"opacity:0.5;\">' + this.buffer.name + '</strong>' +
        '      All L' + this.buffer.lineNumberAtPos() +
        '     (' + humanize(this.buffer.majorMode) + ') ' +
        [].constructor(256).join('-');
};

// The Frame doesn't really own the buffers. The menu-bar is really a function of the active keymap / modes.
function Frame(name, menuBar, minorModes, rootWindow, minibufferWindow, buffers, width, height) {
    this.name = name;
    this.menuBar = menuBar;
    this.minorModes = minorModes;
    this.rootWindow = rootWindow;
    this.minibufferWindow = minibufferWindow;
    this.selectedWindow = this.rootWindow;
    this.buffers = buffers;
    this.width = width || 0;
    this.height = height || 0;
    this.windows = {};
    this.windows[rootWindow.sequenceNumber] = rootWindow;
    this.windows[minibufferWindow.sequenceNumber] = minibufferWindow;
}

Frame.prototype.toViewModel = () => {
    return {'name': this.name,
            'menu-bar': this.menuBar,
            'minor-modes': this.minorModes,
            'root-window': this.rootWindow.toViewModel(this),
            'minibuffer-window': this.minibufferWindow.toViewModel(this)};
};

function BufferText(beg, modiff, saveModiff, markers) {
    this.beg = beg;
    this.modiff = modiff || 0;
    this.saveModiff = saveModiff || 0;
    this.markers = markers || [];
}

BufferText.prototype.nextModificationEvent = (beg) =>
    new BufferText(beg, this.modiff + 1, this.saveModiff, this.markers);

BufferText.prototype.insert = (pt, args) =>
    this.nextModificationEvent(this.beg.insert(pt - 1, args));

BufferText.prototype.deleteRegion = (start, end) =>
    this.nextModificationEvent(this.beg.del(start - 1, end - 1));

function Buffer(name, text, pt, majorMode, minorModes, begv, zv, mark, modeLineFormat) {
    this.name = name;
    this.begv = begv || null;
    this.zv = zv || null;
    this.pt = pt || 1;
    this.mark = mark || null;
    this.newRevision(this.pt, new BufferText(text));
    this.majorMode = majorMode || 'fundamental-mode';
    this.minorModes = minorModes || [];
    this.modeLineFormat = modeLineFormat || '';
}

Buffer.prototype.toViewModel = (frame, win) => {
    let text = this.text.beg,
        lineNumberAtPointMax = text.newlines + 1,
        lineNumberAtPoint = this.lineNumberAtPos(),
        col = this.pt - (text.indexOfLine(lineNumberAtPoint - 1) + 1),
        lineNumberAtStart = this.lineNumberAtPos(win.start),
        lineNumberAtEnd = lineNumberAtStart + win.totalLines - 1,
        lines = text.lines(lineNumberAtStart - 1, lineNumberAtEnd - 1).toString().split(/(?:\r\n?|\n)/gm);
    return {'name': this.name,
            'current': frame.selectedWindow === win,
            'major-mode': this.majorMode,
            'minor-modes': this.minorModes,
            'line-number-at-point-max': lineNumberAtPointMax,
            'line-number-at-point': lineNumberAtPoint,
            'current-column': col,
            'text': lines};
};

Buffer.prototype.lineNumberAtPos = (pos) => {
    pos = (pos || this.pt);
    return this.text.beg.lineAt((pos || this.pt) - 1) + 1;
};

Buffer.prototype.limitToRegion = (position) =>
    Math.max(this.begv || 1, Math.min(position, (this.zv || this.size) + 1));

Buffer.prototype.newRevision = (pt, text) => {
    this.text = text;
    this.size = this.text.beg.length;
    this._revisions = (this._revisions || []).slice(0, this._currentRevision);
    this._revisions.push({text: this.text, pt: pt});
    this._currentRevision = this._revisions.length - 1;
};

Buffer.prototype.narrowToRegion = (start, end) => {
    if (start && end && end < start) {
        let tmp = start;
        start = end;
        end = tmp;
    }
    this.begv = start ? this.limitToRegion(start) : null;
    this.zv = end ? this.limitToRegion(end) : null;
};

Buffer.prototype.widen = () =>
    this.narrowToRegion(null, null);


Buffer.prototype.lookingAt = (regexp) =>
    this.text.beg.charAt(this.pt - 1).match(regexp) || this.text.beg.slice(this.pt - 1).match(regexp);

Buffer.prototype.gotoChar = (position) => {
    this.pt = this.limitToRegion(position);
    return this.pt;
};

Buffer.prototype.forwardChar = (n) =>
    this.gotoChar(this.pt + (n || 1));

Buffer.prototype.backwardChar = (n) =>
    this.gotoChar(this.pt - (n || 1));

Buffer.prototype.insert = (args) => {
    let previousPt = this.pt, nextPt = this.limitToRegion(previousPt + args.length);
    this.newRevision(nextPt, this.text.insert(previousPt, args));
    this.gotoChar(nextPt);
};

Buffer.prototype.deleteRegion = (start, end) => {
    start = this.limitToRegion(start || this.pt);
    end = this.limitToRegion(end || this.mark || this.pt);
    if (end < start) {
        let tmp = end;
        end = start;
        start = tmp;
    }
    this.newRevision(this.pt, this.text.deleteRegion(start, end));
};

Buffer.prototype.deleteForwardChar = (n) => {
    this.deleteRegion(this.pt, this.pt + (n || 1));
};

Buffer.prototype.deleteBackwardChar = (n) => {
    this.backwardChar(n);
    this.deleteForwardChar(n);
};

Buffer.prototype.newline = (n) => {
    n = n || 1;
    while (n > 0) {
        this.insert('\n');
        n -= 1;
    }
};

Buffer.prototype.selfInsertCommand = (arg) => {
    this.insert(arg);
};

Buffer.prototype.undo = (arg) => {
    arg = arg === undefined ? 1 : arg;
    while (arg > 0 && this._currentRevision > 0) {
        this._currentRevision = this._currentRevision - 1;
        this.text = this._revisions[this._currentRevision].text;
        this.size = this.text.beg.length;
        this.gotoChar(this._revisions[this._currentRevision].pt);
        arg -= 1;
    }
};

let scratch = [';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.',
               ';; If you want to create a file, visit that file with C-x C-f,',
               ';; then enter the text in that file\'s own buffer.',
               '',
               ''].join('\n'),
    initalBuffers = {'*scratch*': new Buffer('*scratch*', Rope.toRope(scratch),
                                             scratch.length + 1, 'lisp-interaction-mode'),
                     ' *Minibuf-0*': new Buffer(' *Minibuf-0*', Rope.toRope('Welcome to GNU Emacs'),
                                                1, 'minibuffer-inactive-mode')},
    connections = new Map();

ws.createServer({port: 8080}, (ws) => {
    let id = connections.size + 1,
        frame = new Frame('F' + id,
                          ['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help'],
                          ['blink-cursor-mode', 'menu-bar-mode'],
                          new Window(initalBuffers['*scratch*']),
                          new Window(initalBuffers[' *Minibuf-0*'], true),
                          initalBuffers),
        client = {ws: ws, frame: frame, revision: 0, events: []};
    connections.set(id, client);
    let onrefresh = () => {
            fs.open(path.join(__dirname, 'components.js'), 'r', (err, fd) => {
                if (err) {
                    throw (err);
                }
                client.state = {frame: frame.toViewModel()};
                client.serializedState = JSON.stringify(client.state);
                let data = JSON.stringify(['r', client.revision, client.state, fs.fstatSync(fd).mtime, Date.now()]);
                console.log(' refresh:', data);
                if (ws.readyState === ws.OPEN) {
                    ws.send(data);
                }
            });
        },
        onframesize = (id, width, height) => {
            console.log('    size: frame', id, width, height);
            client.frame.width = width;
            client.frame.height = height;
        },
        onwindowsize = (id, width, height) => {
            console.log('    size: window', id, width, height);
            let win = client.frame.windows[id];
            win.totalCols = width;
            win.totalLines = height;
        },
        onkey = (key) => {
            client.events.push(key);
            console.log('     key:', key);
            let currentBuffer = client.frame.selectedWindow.buffer;
            if (key === 'left') {
                currentBuffer.backwardChar();
            }
            if (key === 'right') {
                currentBuffer.forwardChar();
            }
            if (key === 'return') {
                currentBuffer.newline();
            }
            if (key === 'delete') {
                currentBuffer.deleteForwardChar();
            }
            if (key === 'backspace') {
                currentBuffer.deleteBackwardChar();
            }
            if (key === 'C-/') {
                currentBuffer.undo();
            }
            if (key.length === 1) {
                currentBuffer.selfInsertCommand(key);
            }
            updateClient(client);
        };
    ws.on('close', () => {
        console.log('disconnect:', id);
        connections.delete(id);
    });
    ws.on('message', (data) => {
        let message = JSON.parse(data),
            handler = ({r: onrefresh, k: onkey, zf: onframesize, zw: onwindowsize})[message[0]];
        if (handler) {
            handler.apply(null, message.slice(1));
        } else {
            console.error('unknown message:', message);
        }
    });
    console.log('new client:', id);
    onrefresh();
});

function toSimpleCharDiff(d) {
    if (d.added) {
        return d.value;
    }
    if (d.removed) {
        return -d.value.length;
    }
    return d.value.length;
}


// This fn will be called after a command has been excuted.
function updateClient(client) {
    let startTime = new Date(),
        newState = {frame: client.frame.toViewModel()},
        recentEvents = client.events.slice(-10);

    // Hack to see that we're running for now.
    newState.frame['minibuffer-window'].buffer.text[0] += ' ' + startTime + ' last event at: ' + startTime.getTime() +
        ' last ' + recentEvents.length + ' events: ' + recentEvents.join(' ');

    console.time('    diff');
    let newSerializedState = JSON.stringify(newState),
        diffs = diff.diffChars(client.serializedState, newSerializedState).map(toSimpleCharDiff),
        data = JSON.stringify(['p', client.revision, diffs, startTime.getTime()]);
    client.serializedState = newSerializedState;

    console.timeEnd('    diff');
    console.log(' sending:', data);
    if (client.ws.readyState === ws.OPEN) {
        client.ws.send(data);
        client.revision += 1;
        client.state = newState;
    }
}

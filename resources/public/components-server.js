'use strict';

const diff = require('diff'),
      ws = require('ws'),
      fs = require('fs'),
      path = require('path'),
      Rope = require('./rope').Rope;

function camel(s) {
    return s && s.replace(/-(\w)/g, (_, s) => s.toUpperCase());
}

function humanize (s) {
    return s.split('-').map((s) => s[0].toUpperCase() + s.slice(1)).join(' ');
}

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
    this.pointm = pointm || buffer.pt;
}

Window.nextSequenceNumber = (() => {
    let sequenceNumber = 0;
    return () => {
        sequenceNumber += 1;
        return sequenceNumber;
    };
})();

Window.prototype.toViewModel = (frame) => {
    let lineNumberAtStart = this.buffer ? this.buffer.lineNumberAtPos(this.start) : undefined,
        lineNumberAtPointMax = this.buffer.text.beg.newlines + 1,
        lineNumberAtEnd = Math.min(lineNumberAtPointMax, lineNumberAtStart + this.totalLines) || lineNumberAtPointMax;
    return {'sequence-number': this.sequenceNumber,
            'mini-p': this.isMini,
            'live-p': this.buffer !== undefined,
            'selected': this === frame.selectedWindow,
            'buffer': this.buffer ? this.buffer.toViewModel(frame, this) : undefined,
            'line-number-at-start': lineNumberAtStart,
            'line-number-at-end': lineNumberAtEnd,
            'mode-line': (this.isMini || !this.buffer) ? undefined : this.formatModeLine(frame)};
};

Window.prototype.scrollDown = (arg) =>
    this.scrollUp(-arg || -this.totalLines);

Window.prototype.scrollUp = (arg) => {
    this.buffer.nextLine(arg || this.totalLines);
    this.buffer.beginningOfLine();
};

// Fake, doesn't attempt to take the buffer's mode-line-format into account.
Window.prototype.formatModeLine = (frame) => {
    return '-UUU:----' + frame.name +
        '  <strong style=\"opacity:0.5;\">' + this.buffer.name + '</strong>' +
        '      All L' + this.buffer.lineNumberAtPos() +
        '     (' + humanize(this.buffer.majorMode.replace(/-mode$/, '')) + ') ' +
        [].constructor(256).join('-');
};

// The Frame doesn't really own the buffers. The menu-bar is really a function of the active keymap / modes.
function Frame(name, menuBar, minorModes, rootWindow, minibufferWindow, buffers, globalMap, width, height) {
    this.name = name;
    this.menuBar = menuBar;
    this.minorModes = minorModes;
    this.rootWindow = rootWindow;
    this.minibufferWindow = minibufferWindow;
    this.selectedWindow = this.rootWindow;
    this.buffers = buffers;
    this.globalMap = globalMap;
    this.width = width || 0;
    this.height = height || 0;
    this.closed = false;
    this.windows = {};
    this.windows[rootWindow.sequenceNumber] = rootWindow;
    this.windows[minibufferWindow.sequenceNumber] = minibufferWindow;
}

Frame.prototype.toViewModel = () => {
    return {'name': this.name,
            'menu-bar': this.menuBar,
            'minor-modes': this.minorModes,
            'root-window': this.rootWindow.toViewModel(this),
            'minibuffer-window': this.minibufferWindow.toViewModel(this),
            'closed': this.closed};
};

Frame.prototype.executeExtendedCommand = (prefixArg, command) => {
    console.log(' command:', command);
    let name = camel(command),
        selectedWindow = this.selectedWindow,
        currentBuffer = selectedWindow.buffer,
        target = [this, currentBuffer, selectedWindow].filter((x) => x[name])[0];
    if (target) {
        if (target === currentBuffer) {
            currentBuffer.eventId += 1;
        }
        target[name].call(target, prefixArg);
        selectedWindow.pointm = currentBuffer.pt;
    } else {
        console.error('unknown command:', command);
    }
};

Frame.prototype.saveBuffersKillEmacs = () => {
    this.closed = true;
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

function Buffer(name, text, pt, majorMode, minorModes, mark, modeLineFormat) {
    this.name = name;
    this.pt = pt || 1;
    this.mark = mark || null;
    this.killRing = [];
    this.newRevision(this.pt, new BufferText(text));
    this.majorMode = majorMode || 'fundamental-mode';
    this.minorModes = minorModes || [];
    this.modeLineFormat = modeLineFormat || '';
    this.eventId = 0; // this is a semi-hack.
    this.desiredCol = 0;
}

Buffer.NEW_LINES_PATTERN = /(?:\r\n?|\n)/gm;

Buffer.prototype.toViewModel = (frame, win) => {
    let text = this.text.beg,
        pt = win.pointm,
        lineNumberAtPointMax = text.newlines + 1,
        lineNumberAtPoint = this.lineNumberAtPos(pt),
        col = (pt - (text.indexOfLine(lineNumberAtPoint - 1) + 1)),
        lineNumberAtMark = this.mark && this.lineNumberAtPos(this.mark),
        markCol = this.mark && (this.mark - (text.indexOfLine(lineNumberAtMark - 1) + 1)),
        lineNumberAtStart = this.lineNumberAtPos(win.start),
        lineNumberAtEnd = lineNumberAtStart + win.totalLines,
        lines = text.lines(lineNumberAtStart - 1, lineNumberAtEnd - 1).toString().split(Buffer.NEW_LINES_PATTERN);
    return {'name': this.name,
            'current': frame.selectedWindow === win,
            'major-mode': this.majorMode,
            'minor-modes': this.minorModes,
            'line-number-at-point-max': lineNumberAtPointMax,
            'line-number-at-point': lineNumberAtPoint,
            'line-number-at-mark': lineNumberAtMark,
            'current-column': col,
            'current-mark-column': markCol,
            'text': lines};
};

Buffer.prototype.lineNumberAtPos = (pos) => {
    return this.text.beg.lineAt((pos || this.pt) - 1) + 1;
};

Buffer.prototype.lineBeginningPosition = (n) => {
    n = n === undefined ? 0 : n;
    let line = Math.min(Math.max(1, this.lineNumberAtPos() + n), this.text.beg.newlines + 1);
    return this.text.beg.indexOfLine(line - 1) + 1;
};

Buffer.prototype.lineEndPosition = (n) => {
    n = n === undefined ? 0 : n;
    let line = Math.min(Math.max(1, this.lineNumberAtPos() + n), this.text.beg.newlines + 1),
        lineLength = this.text.beg.line(line - 1).toString().replace(Buffer.NEW_LINES_PATTERN, '').length;
    return this.lineBeginningPosition(n) + lineLength;
};

Buffer.prototype.currentColumn = () => {
    return this.pt - this.lineBeginningPosition();
};

Buffer.prototype.pointMin = () => {
    return 1;
};

Buffer.prototype.pointMax = () => {
    return this.size + 1;
};

Buffer.prototype.limitToRegion = (position) =>
    Math.max(this.pointMin(), Math.min(position, this.pointMax()));

Buffer.prototype.newRevision = (pt, text, singleChar, newline) => {
    this.text = text;
    this.size = this.text.beg.length;
    this.mark = null;
    this._revisions = (this._revisions || []).slice(0, this._currentRevision + 1);
    this._revisions.push({text: this.text, pt: pt, singleChar: singleChar, newline: newline});
    this._currentRevision = this._revisions.length - 1;
};

Buffer.prototype.lookingAt = (regexp) =>
    this.text.beg.charAt(this.pt - 1).match(regexp);

Buffer.prototype.gotoChar = (position) => {
    this.pt = this.limitToRegion(position);
    this.desiredCol = this.currentColumn();
    return this.pt;
};

Buffer.prototype.forwardChar = (n) =>
    this.gotoChar(this.pt + (n === undefined ? 1 : n));

Buffer.prototype.backwardChar = (n) =>
    this.gotoChar(this.pt - (n === undefined ? 1 : n));

Buffer.prototype.forwardWord = (n) => {
    n = n === undefined ? 1 : n;
    while (n > 0) {
        let regexps = [/\W/, /\w/];
        for (let i = 0; i < regexps.length; i += 1) {
            while ((this.lookingAt(regexps[i]))) {
                let previousPt = this.pt;
                if (previousPt === this.forwardChar()) {
                    return;
                }
            }
        }
        n -= 1;
    }
};

Buffer.prototype.backwardWord = (n) => {
    n = n === undefined ? 1 : n;
    while (n > 0) {
        let regexps = [/\W/, /\w/];
        for (let i = 0; i < regexps.length; i += 1) {
            do {
                let previousPt = this.pt;
                if (previousPt === this.backwardChar()) {
                    return;
                }
            } while ((this.lookingAt(regexps[i])))
        }
        this.forwardChar();
        n -= 1;
    }
};

// There's a bug when there's a single line paragraph on the first line of the buffer, jumping too far.
Buffer.prototype.forwardParagraph = (n) => {
    n = n === undefined ? 1 : n;
    while (n > 0) {
        let regexps = [/^\s+$/, /\S/], line;
        for (let i = 0; i < regexps.length; i += 1) {
            do {
                let previousPt = this.pt;
                if (previousPt === this.nextLine()) {
                    return;
                }
                line = this.lineNumberAtPos();
            } while ((this.text.beg.line(line - 1).toString().match(regexps[i])))
        }
        n -= 1;
    }
};

Buffer.prototype.backwardParagraph = (n) => {
    n = n === undefined ? 1 : n;
    while (n > 0) {
        let regexps = [/^\s+$/, /\S/], line;
        for (let i = 0; i < regexps.length; i += 1) {
            do {
                let previousPt = this.pt;
                if (previousPt === this.previousLine()) {
                    return;
                }
                line = this.lineNumberAtPos();
            } while ((this.text.beg.line(line - 1).toString().match(regexps[i])))
        }
        n -= 1;
    }
};

Buffer.prototype.beginningOfBuffer = () =>
    this.gotoChar(this.pointMin());

Buffer.prototype.endOfBuffer = () =>
    this.gotoChar(this.pointMax());

Buffer.prototype.beginningOfLine = (n) =>
    this.gotoChar(this.lineBeginningPosition(n === undefined ? 0 : n));

Buffer.prototype.endOfLine = (n) =>
    this.gotoChar(this.lineEndPosition(n === undefined ? 0 : n));

Buffer.prototype.nextLine = (n) => {
    let col = this.desiredCol;
    this.beginningOfLine(n === undefined ? 1 : n);
    let text = this.text.beg,
        lineLength = text.line(this.lineNumberAtPos() - 1).toString().replace(Buffer.NEW_LINES_PATTERN, '').length;
    this.forwardChar(Math.min(col, lineLength));
    this.desiredCol = col;
    return this.pt;
};

Buffer.prototype.previousLine = (n) =>
    this.nextLine(-(n === undefined ? 1 : n));

Buffer.prototype.insert = (args) => {
    let previousPt = this.pt,
        nextPt = previousPt + args.length,
        newline = args === '\n';
    this.newRevision(this.pt, this.text.insert(this.pt, args), args.length === 1 && !newline, newline);
    return this.gotoChar(nextPt);
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
    return this.gotoChar(start);
};

Buffer.prototype.bufferSubstring = (start, end) => {
    start = this.limitToRegion(start || this.pt);
    end = this.limitToRegion(end || this.mark || this.pt);
    if (end < start) {
        let tmp = end;
        end = start;
        start = tmp;
    }
    return this.text.beg.slice(start - 1, end - 1).toString();
};

Buffer.prototype.bufferString = () => this.text.beg.toString();

Buffer.prototype.deleteForwardChar = (n) => {
    this.mark = null;
    this.setMarkCommand();
    this.forwardChar(n);
    return this.deleteRegion();
};

Buffer.prototype.deleteBackwardChar = (n) => {
    if (!this.mark) {
        this.setMarkCommand();
        this.backwardChar(n);
    }
    return this.deleteRegion();
};

Buffer.prototype.killRegion = (start, end) => {
    this.killRing.push({text: this.bufferSubstring(start, end), type: 'kill-region', id: this.eventId});
    return this.deleteRegion(start, end);
};

Buffer.prototype.killLine = (n) => {
    let previousPt = this.pt;
    this.setMarkCommand();
    if (previousPt === this.endOfLine(n)) {
        this.beginningOfLine(1);
    }
    this.exchangePointAndMark();
    this.killRing.push({text: this.bufferSubstring(), type: 'kill-line', id: this.eventId});
    return this.deleteRegion();
};

Buffer.prototype.killWord = (n) => {
    this.setMarkCommand();
    this.forwardWord(n);
    this.exchangePointAndMark();
    this.killRing.push({text: this.bufferSubstring(), type: 'kill-word', id: this.eventId});
    return this.deleteRegion();
};

Buffer.prototype.backwardKillWord = (n) => {
    this.setMarkCommand();
    this.backwardWord(n);
    this.exchangePointAndMark();
    let mark = this.mark;
    this.killRing.push({text: this.bufferSubstring(), type: 'backward-kill-word', id: this.eventId});
    this.deleteRegion();
    return this.gotoChar(mark);
};

Buffer.prototype.newline = (n) => {
    n = n === undefined ? 1 : n;
    while (n > 0) {
        this.insert('\n');
        n -= 1;
    }
};

Buffer.prototype.openLine = (n) => {
    let previousPt = this.pt;
    this.newline(n);
    return this.gotoChar(previousPt);
};

Buffer.prototype.yank = (n) => {
    n = n === undefined ? 1 : n;
    let previous, kill, text = '';

    do {
        kill = this.killRing[this.killRing.length - n];
        if (kill) {
            if (previous && Math.abs(previous.id - kill.id) !== 1) {
                break;
            }
            if (kill.type === 'backward-kill-word') {
                text += kill.text;
            } else {
                text = kill.text + text;
            }
            previous = kill;
            n += 1;
        }
    } while (kill);
    if (text.length > 0) {
        this.insert(text);
    }
};

Buffer.prototype.selfInsertCommand = (arg) => {
    arg = arg === undefined ? 1 : arg;
    return this.insert([].constructor(arg + 1).join(this.lastCommandEvent));
};

Buffer.prototype.undo = (arg) => {
    arg = arg === undefined ? 1 : arg;
    while (arg > 0 && this._currentRevision > 0) {
        let befreUndo = this._revisions[this._currentRevision];
        this._currentRevision = this._currentRevision - 1;
        let afterUndo = this._revisions[this._currentRevision];
        this.text = afterUndo.text;
        this.size = this.text.beg.length;
        this.gotoChar(befreUndo.pt);
        if (!(befreUndo.singleChar && afterUndo.singleChar || (befreUndo.newline && afterUndo.newline))) {
            arg -= 1;
        }
    }
};

Buffer.prototype.setMarkCommand = () => {
    this.mark = this.mark === this.pt ? null : this.pt;
};

Buffer.prototype.markWholeBuffer = () => {
    this.gotoChar(this.size + 1);
    this.setMarkCommand();
    this.gotoChar(1);
};

Buffer.prototype.markWord = (arg) => {
    if (!this.mark) {
        this.setMarkCommand();
    }
    this.exchangePointAndMark();
    this.forwardWord(arg);
    this.exchangePointAndMark();
};

Buffer.prototype.markParagraph = (arg) => {
    if (!this.mark) {
        this.backwardParagraph();
        this.setMarkCommand();
    }
    this.exchangePointAndMark();
    this.forwardParagraph(arg);
    this.exchangePointAndMark();
};

Buffer.prototype.exchangePointAndMark = () => {
    if (this.mark) {
        let tmp = this.mark;
        this.mark = this.pt;
        this.pt = tmp;
    }
};

let scratch = [';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.',
               ';; If you want to create a file, visit that file with C-x C-f,',
               ';; then enter the text in that file\'s own buffer.',
               '',
               ''].join('\n');

function initalBuffers() {
    return {'*scratch*': new Buffer('*scratch*', Rope.toRope(scratch),
                                    scratch.length + 1, 'lisp-interaction-mode'),
            ' *Minibuf-0*': new Buffer(' *Minibuf-0*', Rope.toRope('Welcome to GNU Emacs'),
                                       1, 'minibuffer-inactive-mode'),
            ' *Echo Area 0*': new Buffer(' *Echo Area 0*', Rope.EMPTY, 1, 'fundamental-mode')};
}

function defaultKeyMap() {
    return {'left': 'backward-char',
            'C-b': 'backward-char',
            'right': 'forward-char',
            'C-f': 'forward-char',
            'up': 'previous-line',
            'C-p': 'previous-line',
            'down': 'next-line',
            'C-n': 'next-line',
            'prior': 'scroll-down',
            'M-v': 'scroll-down',
            'next': 'scroll-up',
            'C-v': 'scroll-up',
            'C-left': 'backward-word',
            'M-left': 'backward-word',
            'C-right': 'forward-word',
            'M-right': 'forward-word',
            'C-backspace': 'backward-kill-word',
            'M-backspace': 'backward-kill-word',
            'C-delete': 'kill-word',
            'M-delete': 'kill-word',
            'M-@': 'mark-word',
            'M-h': 'mark-paragraph',
            'C-k': 'kill-line',
            'C-w': 'kill-region', // closes tab in Chrome.
            'C-y': 'yank',
            'C-up': 'backward-paragraph',
            'C-down': 'forward-paragraph',
            'return': 'newline',
            'C-m': 'newline',
            'C-o': 'open-line',
            'delete': 'delete-forward-char',
            'C-d': 'delete-forward-char',
            'backspace': 'delete-backward-char',
            'C-a': 'beginning-of-line',
            'home': 'beginning-of-line',
            'C-e': 'end-of-line',
            'end': 'end-of-line',
            'C-home': 'beginning-of-buffer',
            'C-end': 'end-of-buffer',
            'C-/': 'undo',
            'C-_': 'undo',
            'C- ': 'set-mark-command',
            'C-x': {'C-c': 'save-buffers-kill-emacs',
                    'h': 'mark-whole-buffer'}};
}

function initialFrame(id) {
    let buffers = initalBuffers();
    return new Frame('F' + id,
                     ['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help'],
                     ['blink-cursor-mode', 'menu-bar-mode'],
                     new Window(buffers['*scratch*']),
                     new Window(buffers[' *Minibuf-0*'], true),
                     buffers, defaultKeyMap());
}

let connections = new Map();

ws.createServer({port: 8080}, (ws) => {
    let id = connections.size + 1,
        frame = initialFrame(id),
        client = {ws: ws, frame: frame, revision: 0, events: []},
        onrefresh = () => {
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
            console.log('    keys:', client.events);
            let command = client.events.reduce((map, key) => map && map[key], frame.globalMap),
                prefixArg;

            if (!command && key.length === 1 && client.events.length === 1) {
                command = 'self-insert-command';
            }

            if (typeof command !== 'object') {
                client.events = [];
            }

            if (typeof command === 'string') {
                let currentBuffer = frame.selectedWindow.buffer;
                currentBuffer.lastCommandEvent = key;
                try {
                    frame.executeExtendedCommand(prefixArg, command);
                    updateClient(client);
                } finally {
                    delete currentBuffer.lastCommandEvent;
                }
            }
        };
    connections.set(id, client);
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
        newState = {frame: client.frame.toViewModel()};

    console.time('  update');
    let newSerializedState = JSON.stringify(newState);

    if (newSerializedState !== client.serializedState) {
        let diffs = diff.diffChars(client.serializedState, newSerializedState).map(toSimpleCharDiff),
            data = JSON.stringify(['p', client.revision, diffs, startTime.getTime()]);

        if (client.ws.readyState === ws.OPEN) {
            console.log(' sending:', data);
            client.ws.send(data);
            client.revision += 1;
            client.state = newState;
            client.serializedState = newSerializedState;
        }
    }
    console.timeEnd('  update');
}

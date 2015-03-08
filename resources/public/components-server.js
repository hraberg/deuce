'use strict';

let SEND_ENTIRE_BUFFER = false,
    REFRESH_THRESHOLD = 4096;

const DiffMatchPatch = require('googlediff'),
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
                totallLines, totalCols, normalLines, normalCols, start, pointm, contextLines) {
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
    this.totalLines = totallLines || 0;
    this.totalCols = totalCols || 0;
    this.normalLines = normalLines;
    this.normalCols = normalCols;
    this.start = start || 1;
    this.pointm = pointm || buffer.pt;
    this.contextLines = contextLines || 2;
    this.buffer.win = this;
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
        lineNumberAtEnd = Math.min(lineNumberAtPointMax, lineNumberAtStart + this.totalLines),
        isLive = this.buffer !== undefined;
    return {'sequence-number': this.sequenceNumber,
            'mini-p': this.isMini,
            'live-p': isLive,
            'selected': this === frame.selectedWindow,
            'buffer': this.buffer ? this.buffer.toViewModel(frame, this) : undefined,
            'line-number-at-start': lineNumberAtStart,
            'line-number-at-end': lineNumberAtEnd,
            'mode-line': isLive && !this.isMini ? this.formatModeLine() : undefined};
};

Window.prototype.scrollDown = (arg) => {
    this.buffer.previousLine(arg || this.totalLines);
    this.buffer.beginningOfLine();
    this.pointm = this.buffer.pt;
    let lineNumberAtPoint = this.buffer.lineNumberAtPos(),
        startLine = Math.max(1, lineNumberAtPoint - (this.totalLines - 1 - this.contextLines));
    this.start = this.buffer.text.beg.indexOfLine(startLine - 1) + 1;
};

// There's a bug here at the final page where the start line doesn't always feels right.
Window.prototype.scrollUp = (arg) => {
    this.buffer.nextLine(arg || this.totalLines);
    this.buffer.beginningOfLine();
    this.pointm = this.buffer.pt;
    let lineNumberAtStart = this.buffer.lineNumberAtPos(this.start),
        lineNumberAtPointMax = this.buffer.text.beg.newlines + 1;
    if (lineNumberAtStart + this.totalLines >= lineNumberAtPointMax) {
        this.recenter();
    } else {
        this.start = this.pointm;
    }
};

Window.prototype.setBuffer = (buffer) => {
    this.buffer.lastVisibleStart = this.start;
    this.buffer = buffer;
    this.pointm = buffer.pt;
    this.start = buffer.lastVisibleStart || 1;
    buffer.win = this;
};

Window.prototype.recenter = (arg) => {
    let lineNumberAtPoint = this.buffer.lineNumberAtPos(),
        lineNumberAtStart = this.buffer.lineNumberAtPos(this.start),
        startLine = Math.max(1, lineNumberAtPoint - Math.round((this.totalLines - this.contextLines) / 2));
    if (arg !== undefined) {
        startLine = lineNumberAtStart + arg > 0 ? Math.min(this.totalLines, arg) : Math.max(this.totallLines - 1, arg);
        startLine = Math.min(Math.max(startLine, this.buffer.lineNumberAtPos(this.buffer.pointMin())),
                             this.buffer.lineNumberAtPos(this.buffer.pointMax()));
    } else if (lineNumberAtStart === lineNumberAtPoint && this.frame.lastCommand === 'recenter') {
        startLine = Math.max(lineNumberAtPoint - this.totalLines + this.contextLines,
                             this.buffer.lineNumberAtPos(this.buffer.pointMin()));
    } else if (lineNumberAtStart === startLine && this.frame.lastCommand === 'recenter') {
        startLine = lineNumberAtPoint;
    }
    this.start = this.buffer.text.beg.indexOfLine(startLine - 1) + 1;

};

Window.prototype.adjustScroll = () => {
    let lineNumberAtStart = this.buffer.lineNumberAtPos(this.start),
        lineNumberAtPointMax = this.buffer.text.beg.newlines + 1,
        lineNumberAtVisibleEnd = Math.min(lineNumberAtPointMax, lineNumberAtStart + this.totalLines - this.contextLines - 1),
        windowVisibleEnd = this.buffer.text.beg.indexOfLine(lineNumberAtVisibleEnd - 1) + 1;
    if (this.start > this.pointm || windowVisibleEnd < this.pointm) {
        this.recenter();
    }
};

// Fake, doesn't attempt to take the buffer's mode-line-format into account.
Window.prototype.formatModeLine = () => {
    let codingSystem = '-',
        endOfLineStyle = ':',
        readOnlyMode = this.buffer.minorModes.indexOf('read-only-mode') !== -1,
        modified = this.buffer.bufferModifiedP() ? '*' : '-',
        writable = readOnlyMode ? '%' : modified,
        localDirectory = '-',
        lineNumberAtStart = this.buffer.lineNumberAtPos(this.start),
        lineNumberAtPoint = this.buffer.lineNumberAtPos(),
        lineNumberAtPointMax = this.buffer.lineNumberAtPos(this.buffer.pointMax()),
        modes = [this.buffer.majorMode].concat(this.buffer.minorModes).map((m) => m.replace(/-mode$/, '')).map(humanize),
        location = (this.totalLines >= lineNumberAtPointMax || lineNumberAtPoint > this.totallLines
                    ? 'All' : lineNumberAtStart === 1
                    ? 'Top' : lineNumberAtStart + this.totalLines > lineNumberAtPointMax
                    ? 'Bot' : Math.round(lineNumberAtPoint / lineNumberAtPointMax * 100) + '%'),
        line = (lineNumberAtPoint + '     ').slice(0, 6);
    return codingSystem + endOfLineStyle + writable + modified + localDirectory +
        '  ' + '<strong style=\"opacity:0.5;\">' + this.buffer.name + '</strong>' +
        '      ' + ('    ' + location).slice(-4) + ' ' + 'L' + line + '(' + modes.join(' ') + ')' +
        ' ' + [].constructor(this.totalCols).join('-');
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
    rootWindow.frame = this;
    minibufferWindow.frame = this;
    this.recursiveEditLevel = 0;
}

Frame.prototype.toViewModel = () => {
    return {'name': this.name,
            'menu-bar': this.menuBar,
            'minor-modes': this.minorModes,
            'root-window': this.rootWindow.toViewModel(this),
            'minibuffer-window': this.minibufferWindow.toViewModel(this),
            'closed': this.closed};
};

Frame.prototype.prompt = (prompt) => {
    this.noEcho = true;
    let minibuffer = this.buffers[' *Minibuf-' + this.recursiveEditLevel + '*'];
    this.minibufferWindow.setBuffer(minibuffer);
    minibuffer.markWholeBuffer();
    minibuffer.insert(prompt);
    this.minibufferWindow.pointm = minibuffer.pt;
    minibuffer.majorMode = 'fundamental-mode';
    this.selectedWindow = this.minibufferWindow;
    this.noEcho = false;
};

Frame.prototype.executeExtendedCommand = (prefixArg, command) => {
    console.log(' command:', command);
    if (command === 'execute-extended-command') {
        return this.prompt('M-x ');
    }
    let name = camel(command),
        selectedWindow = this.selectedWindow,
        currentBuffer = selectedWindow.buffer,
        target = [this, currentBuffer, selectedWindow].filter((x) => x[name])[0];
    if (target) {
        if (target === currentBuffer) {
            currentBuffer.eventId += 1;
        }
        if (this.minibufferWindow.buffer === this.buffers[' *Echo Area ' + this.recursiveEditLevel + '*']) {
            this.minibufferWindow.setBuffer(this.buffers[' *Minibuf-' + this.recursiveEditLevel + '*']);
        }
        this.thisCommand = command;
        target[name].call(target, prefixArg);
        this.lastCommand = this.thisCommand;
        this.thisCommand = null;
        if (target === currentBuffer) {
            selectedWindow.pointm = currentBuffer.pt;
            selectedWindow.adjustScroll();
        }
    } else {
        throw new Error('`' + command + '\' is not a valid command name');
    }
};

Frame.prototype.saveBuffersKillEmacs = () => {
    this.closed = true;
};

['menu-bar-mode', 'scroll-bar-mode', 'blink-cursor-mode']
    .forEach((mode) =>
             Frame.prototype[camel(mode)] = () => {
                 let idx = this.minorModes.indexOf(mode);
                 if (idx === -1) {
                     this.minorModes.push(mode);
                 } else {
                     this.minorModes.splice(idx, 1);
                 }
             });

// Assumes only two windows atm.
Frame.prototype.otherWindow = () => {
    if (this.selectedWindow === this.minibufferWindow) {
        this.selectedWindow = this.rootWindow;
    } else if (this.selectedWindow === this.rootWindow && this.minibufferWindow.majorMode !== 'minibuffer-inactive-mode') {
        this.selectedWindow = this.minibufferWindow;
    }
};

Frame.prototype.message = (str) => {
    if (this.noEcho) {
        return;
    }
    this.noEcho = true;
    let echoArea = this.buffers[' *Echo Area ' + this.recursiveEditLevel + '*'];
    this.minibufferWindow.setBuffer(echoArea);
    echoArea.markWholeBuffer();
    echoArea.insert(str);
    this.noEcho = false;
};

Frame.prototype.keyboardQuit = () => {
    if (this.selectedWindow.isMini) {
        this.noEcho = true;
        let minibuffer = this.selectedWindow.buffer;
        minibuffer.markWholeBuffer();
        minibuffer.deleteRegion();
        minibuffer.majorMode = 'minibuffer-inactive-mode';
        this.selectedWindow.pointm = minibuffer.pt;
        this.selectedWindow = this.rootWindow;
        this.noEcho = false;
    }
    throw new Error('Quit');
};

// This just picks another buffer, no prompts yet.
Frame.prototype.switchToBuffer = () => {
    let selectedWindow = this.selectedWindow,
        currentBuffer = selectedWindow.buffer,
        otherBufferName = Object.keys(this.buffers).filter((name) => name !== currentBuffer.name && !name.match(/^ /))[0];
    if (otherBufferName) {
        selectedWindow.setBuffer(this.buffers[otherBufferName]);
    }
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

function Buffer(name, text, pt, majorMode, minorModes, mark, modeLineFormat, tabWidth) {
    this.name = name;
    this.pt = pt || 1;
    this.mark = mark || null;
    this.killRing = [];
    this.newRevision(this.pt, new BufferText(text));
    this.majorMode = majorMode || 'fundamental-mode';
    this.minorModes = minorModes || [];
    this.modeLineFormat = modeLineFormat || '';
    this.tabWidth = tabWidth || 8;
    this.eventId = 0; // this is a semi-hack.
    this.desiredCol = 0;
}

Buffer.NEW_LINES_PATTERN = /(?:\r\n?|\n)/gm;

Buffer.prototype.expandTab = (line, col, visibleCol) => {
    return line[col] === '\t' ? (this.tabWidth - (visibleCol % this.tabWidth)) : 1;
};

Buffer.prototype.lineVisibleColumn = (line, col) => {
    let visibleCol = 0;
    line = this.text.beg.line(line - 1).toString();
    for (let i = 0; i < col; i += 1) {
        visibleCol += this.expandTab(line, i, visibleCol);
    }
    return visibleCol;
};

Buffer.prototype.visibleColumnToColumn = (line, visibleCol) => {
    let col, i = 0;
    line = this.text.beg.line(line - 1).toString();
    for (col = 0; col < line.length && i < visibleCol; col += 1) {
        i += this.expandTab(line, col, i);
    }
    if (Math.min(Math.max(i, 0), visibleCol) % this.tabWidth !== 0 && line[col - 1] === '\t') {
        return col - 1;
    }
    return col;
};

Buffer.prototype.toViewModel = (frame, win) => {
    let text = this.text.beg,
        pt = win.pointm,
        lineNumberAtPointMax = text.newlines + 1,
        lineNumberAtPoint = this.lineNumberAtPos(pt),
        col = (pt - (text.indexOfLine(lineNumberAtPoint - 1) + 1)),
        markActive = this.mark !== null,
        lineNumberAtMark = markActive && this.lineNumberAtPos(this.mark),
        markCol = markActive && (this.mark - (text.indexOfLine(lineNumberAtMark - 1) + 1)),
        lineNumberAtStart = this.lineNumberAtPos(win.start),
        lineNumberAtEnd = lineNumberAtStart + win.totalLines,
        lines;

    if (SEND_ENTIRE_BUFFER) {
        lines = text.toString().split(Buffer.NEW_LINES_PATTERN);
    } else {
       lines = text.lines(lineNumberAtStart - 1, lineNumberAtEnd - 1).toString().split(Buffer.NEW_LINES_PATTERN);
    }

    col = this.lineVisibleColumn(lineNumberAtPoint, col);

    if (markCol) {
        markCol = this.lineVisibleColumn(lineNumberAtMark, markCol);
    }

    return {'name': this.name,
            'current': frame.selectedWindow === win,
            'major-mode': this.majorMode,
            'minor-modes': this.minorModes,
            'line-number-at-point-max': lineNumberAtPointMax,
            'line-number-at-point': lineNumberAtPoint,
            'line-number-at-mark': lineNumberAtMark,
            'current-column': col,
            'current-mark-column': markCol,
            'mark-active': markActive,
            'tab-width': this.tabWidth,
            'text': lines};
};

Buffer.prototype.bufferModifiedP = () => this.text.modiff !== this.text.saveModiff;

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
    if ((this.minorModes || []).indexOf('read-only-mode') !== -1) {
        return; // should probably throw an exception which gets handled in the command loop.
    }
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
    if (position === this.pt) {
        return this.pt;
    }
    let previousPt = this.pt;
    this.pt = this.limitToRegion(position);
    this.desiredCol = this.lineVisibleColumn(this.lineNumberAtPos(), this.currentColumn());
    if (previousPt === this.pointMin() && this.pt === this.pointMin()) {
        this.win.frame.message('Beginning of buffer');
    }
    if (previousPt === this.pointMax() && this.pt === this.pointMax()) {
        this.win.frame.message('End of buffer');
    }
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

Buffer.prototype.beginningOfLine = (n) => {
    n = n === undefined ? 0 : n;
    if (n + this.lineNumberAtPos() < this.lineNumberAtPos(this.pointMin())) {
        this.win.frame.message('Beginning of buffer');
    } else if (n + this.lineNumberAtPos() > this.lineNumberAtPos(this.pointMax())) {
        this.win.frame.message('End of buffer');
    }
    this.gotoChar(this.lineBeginningPosition(n));
};

Buffer.prototype.endOfLine = (n) => {
    this.beginningOfLine(n === undefined ? 0 : -n);
    this.gotoChar(this.lineEndPosition());
};

Buffer.prototype.nextLine = (n) => {
    let previousDesiredCol = this.desiredCol;
    this.beginningOfLine(n === undefined ? 1 : n);
    let text = this.text.beg,
        lineLength = text.line(this.lineNumberAtPos() - 1).toString().replace(Buffer.NEW_LINES_PATTERN, '').length,
        col = this.visibleColumnToColumn(this.lineNumberAtPos(), previousDesiredCol);
    this.forwardChar(Math.min(col, lineLength));
    this.desiredCol = previousDesiredCol;
    return this.pt;
};

Buffer.prototype.previousLine = (n) =>
    this.nextLine(-(n === undefined ? 1 : n));

Buffer.prototype.insert = (args) => {
    if (this.mark) {
        let start = Math.min(this.mark, this.pt),
            end = Math.max(this.mark, this.pt);
        this.newRevision(this.pt, this.text.deleteRegion(start, end).insert(start, args));
        return this.gotoChar(start + args.length);
    } else {
        let nextPt = this.pt + args.length,
            newline = args === '\n',
            singleChar = this.win.frame.thisCommand === 'self-insert-command' && !newline;
        this.newRevision(this.pt, this.text.insert(this.pt, args), singleChar, newline);
        return this.gotoChar(nextPt);
    }
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
    this.exchangePointAndMark();
    return this.deleteRegion();
};

Buffer.prototype.deleteBackwardChar = (n) => {
    if (!this.mark || this.mark === this.pt) {
        this.mark = null;
        this.setMarkCommand();
        this.backwardChar(n);
        this.exchangePointAndMark();
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
     // super hack - just to get going, it should have its own key map etc.
    if (this.win.isMini) {
        let command = this.bufferString().replace(/^M-x/, '').trim();
        try {
            this.win.frame.keyboardQuit();
        } catch (ignore) {
        }
        this.win.frame.executeExtendedCommand(null, command);
        return;
    }
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
    return this.insert([].constructor(arg + 1).join(this.win.frame.lastCommandEvent));
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
        this.win.frame.message('Undo!');
        if (!(befreUndo.singleChar && afterUndo.singleChar || (befreUndo.newline && afterUndo.newline))) {
            arg -= 1;
        }
    }
};

Buffer.prototype.setMarkCommand = () => {
    this.mark = this.mark === this.pt ? null : this.pt;
    if (this.win.frame.thisCommand === 'set-mark-command') {
        this.win.frame.message(this.mark ? 'Mark set' : 'Mark cleared');
    }
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

['fundamental-mode', 'lisp-interaction-mode']
    .forEach((mode) => Buffer.prototype[camel(mode)] = () => this.majorMode = mode);

let scratch = [';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.',
               ';; If you want to create a file, visit that file with C-x C-f,',
               ';; then enter the text in that file\'s own buffer.',
               '',
               ''].join('\n'),
    tutorial = fs.readFileSync(path.join(__dirname, '/../etc/tutorials/TUTORIAL'), {encoding: 'utf8'});

function initalBuffers() {
    return {'*scratch*': new Buffer('*scratch*', Rope.toRope(scratch),
                                    scratch.length + 1, 'lisp-interaction-mode'),
            'TUTORIAL': new Buffer('TUTORIAL', Rope.toRope(tutorial), 1, 'fundamental-mode'),
            ' *Minibuf-0*': new Buffer(' *Minibuf-0*', Rope.EMPTY,
                                       1, 'minibuffer-inactive-mode'),
            ' *Echo Area 0*': new Buffer(' *Echo Area 0*', Rope.EMPTY, 1, 'minibuffer-inactive-mode')};
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
            'C-l': 'recenter',
            'C-left': 'backward-word',
            'M-left': 'backward-word',
            'M-b': 'backward-word',
            'C-right': 'forward-word',
            'M-right': 'forward-word',
            'M-f': 'forward-word',
            'C-backspace': 'backward-kill-word',
            'M-backspace': 'backward-kill-word',
            'C-delete': 'kill-word',
            'M-delete': 'kill-word',
            'M-@': 'mark-word',
            'M-h': 'mark-paragraph',
            'C-k': 'kill-line',
            'C-w': 'kill-region', // has onbeforeonload hack to avoid closing tab in Chrome.
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
            'M-<': 'beginning-of-buffer',
            'C-end': 'end-of-buffer',
            'M->': 'end-of-buffer',
            'C-/': 'undo',
            'C-_': 'undo',
            'C- ': 'set-mark-command',
            'C-g': 'keyboard-quit',
            'M-x': 'execute-extended-command',
            'C-x': {'C-c': 'save-buffers-kill-emacs',
                    'b': 'switch-to-buffer',
                    'h': 'mark-whole-buffer',
                    'o': 'other-window',
                    'u': 'undo'}};
}

function initialFrame(id) {
    let buffers = initalBuffers();
    return new Frame('F' + id,
                     ['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help'],
                     ['blink-cursor-mode', 'menu-bar-mode', 'scroll-bar-mode'],
                     new Window(buffers['*scratch*']),
                     new Window(buffers[' *Minibuf-0*'], true),
                     buffers, defaultKeyMap());
}

let dmp = new DiffMatchPatch();

function diffLineMode(from, to) {
    let a = dmp.diff_linesToChars_(from, to),
        diffs = dmp.diff_main(a.chars1, a.chars2, false);
    dmp.diff_charsToLines_(diffs, a.lineArray);
    return diffs;
}

function diffCharMode(from, to) {
    return dmp.diff_main(from, to);
}

let diff = SEND_ENTIRE_BUFFER ? diffCharMode : diffLineMode;

function toSimpleDiff(d) {
    if (d[0] === 1) {
        return d[1];
    }
    if (d[0] === -1) {
        return -d[1].length;
    }
    return d[1].length;
}

function serialize(state) {
    if (SEND_ENTIRE_BUFFER) {
        return JSON.stringify(state);
    } else {
        return JSON.stringify(state, null, 1);
    }
}

function updateClient(client) {
    let startTime = new Date(),
        newState = {frame: client.frame.toViewModel()};

    let newSerializedState = serialize(newState);

    if (Math.abs(newSerializedState.length - client.serializedState.length) > REFRESH_THRESHOLD) {
        return refreshClient(client, newState, newSerializedState);
    }

    if (newSerializedState !== client.serializedState) {
        console.time('  update');
        let diffs = diff(client.serializedState, newSerializedState).map(toSimpleDiff),
            data = JSON.stringify(['p', client.revision, diffs, startTime.getTime()]);

        if (client.ws.readyState === ws.OPEN) {
            console.log(' sending:', data);
            client.ws.send(data);
            client.revision += 1;
            client.state = newState;
            client.serializedState = newSerializedState;
        }
        console.timeEnd('  update');
    }
}

function refreshClient(client, newState, newSerializedState) {
    console.time(' refresh:');
    client.state = newState || {frame: client.frame.toViewModel()};
    client.serializedState = newSerializedState || serialize(client.state);
    let data = JSON.stringify(['r', client.revision, client.serializedState, Date.now()]);
    if (client.ws.readyState === ws.OPEN) {
        console.log(' sending:', data);
        client.ws.send(data);
    }
    console.timeEnd(' refresh:');
}

let connections = new Map();

ws.createServer({port: 8080}, (ws) => {
    let id = connections.size + 1,
        frame = initialFrame(id),
        client = {ws: ws, frame: frame, revision: 0, events: []},
        onrefresh = () => refreshClient(client),
        onframesize = (id, width, height) => {
            console.log('    size: frame', id, width, height);
            client.frame.width = width;
            client.frame.height = height;
            updateClient(client);
        },
        onwindowsize = (id, width, height) => {
            console.log('    size: window', id, width, height);
            let win = client.frame.windows[id];
            win.totalCols = width;
            win.totalLines = height;
            updateClient(client);
        },
        onwindowscroll = (id, lineNumberAtStart) => {
            console.log('  scroll: window', id, lineNumberAtStart);
            let win = client.frame.windows[id];
            win.start = win.buffer.text.beg.indexOfLine(lineNumberAtStart - 1) + 1;
            let pt = Math.min(Math.max(win.start, win.pointm),
                              win.buffer.text.beg.indexOfLine(lineNumberAtStart - 1 + (win.totalLines - 2 - 1 )) + 1);
            win.buffer.gotoChar(pt);
            win.buffer.beginningOfLine();
            win.pointm = win.buffer.pt;
            updateClient(client);
        },
        onkey = (key) => {
            if (client.keyTimer) {
                clearTimeout(client.keyTimer);
                delete client.keyTimer;
            }
            client.events.push(key);
            console.log('    keys:', client.events);

            let command;
            if (client.events[0] === 'C-u') {
                command = {};
                if (client.events.length === 1) {
                    client.prefixArg = 0;
                    client.prefixSign = 1;
                } else if (client.events.length === 2 && key === '-') {
                    client.prefixSign = -1;
                } else if (key.length === 1 && (!Number.isNaN(parseInt(key, 10)) || key === '-')) {
                    client.prefixArg = client.prefixArg * 10 + parseInt(key, 10);
                } else {
                    client.events = client.events.slice(-1);
                    command = null;
                }
            }

            if (!command) {
                command = client.events.reduce((map, key) => map && map[key], frame.globalMap);
            }

            if (!command && key.length === 1 && client.events.length === 1) {
                command = 'self-insert-command';
            }

            if (!command) {
                frame.message(client.events.join(' ') + ' is undefined');
                updateClient(client);
            }

            if (typeof command !== 'object') {
                client.events = [];
            }

            if (typeof command === 'object') {
                client.keyTimer = setTimeout(() => {
                    frame.message(client.events.join(' ') + '-');
                    updateClient(client);
                }, 1000);
            }

            if (typeof command === 'string') {
                frame.lastCommandEvent = key;
                try {
                    frame.executeExtendedCommand(client.prefixSign ? client.prefixSign * client.prefixArg : undefined, command);
                } catch (e) {
                    frame.message(e.message);
                } finally {
                    delete client.prefixArg;
                    delete client.prefixSign;
                    delete frame.lastCommandEvent;
                    updateClient(client);
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
            handler = ({r: onrefresh, k: onkey, s: onwindowscroll,
                        zf: onframesize, zw: onwindowsize})[message[0]];
        if (handler) {
            handler.apply(null, message.slice(1));
        } else {
            console.error('unknown message:', message);
        }
    });
    console.log('new client:', id);
    frame.message('Welcome to deuce.js');
    onrefresh();
});

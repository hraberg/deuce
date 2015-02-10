/*jslint browser: true */

// This is a spike for the Deuce web UI. In the real world it won't have as much logic on the client side.

// JS to XTerm, see keyPress and keyDown:
//  https://github.com/chjj/term.js/blob/master/src/term.js MIT
// Key handling pulled out:
//  https://github.com/Gottox/terminal.js/blob/master/lib/input/dom.js MIT/X

(function () {
    'use strict';

    var fontWidth, fontHeight,
        keys = {backspace: 8, newline: 10, ret: 13,
                shift: 16, ctrl: 17, alt: 18, esc: 27, left: 37,
                up: 38, right: 39, down: 40,
                ins: 45, del: 46, slash: 191},
        mouseButton = {left: 0, middle: 1, right: 2},
        undoHistory = {},
        killRing;

    function matches(element, selector) {
        return (element.matches || element.mozMatchesSelector).call(element, selector);
    }

    function selectedFrame() {
        return document.querySelector('.selected.frame');
    }

    function selectedWindow() {
        return document.querySelector('.selected.window');
    }

    function currentBuffer() {
        return document.querySelector('.current.buffer');
    }

    function bufferString() {
        return currentBuffer().textContent;
    }

    function point() {
        return currentBuffer().parentElement.querySelector('.point');
    }

    function mark() {
        return currentBuffer().parentElement.querySelector('.mark');
    }

    function bufferLines(offset) {
        var linesAndBreaks = bufferString().substring(0, offset).split(/(\r?\n)/),
            lines = [],
            i;

        for (i = 0; i < linesAndBreaks.length; i += 2) {
            lines.push(linesAndBreaks[i] + (linesAndBreaks[i + 1] || ''));
        }
        return lines;
    }


    function lineLengthNoNewline(line) {
        return (line || '').replace(/(\r?\n)*$/, '').length;
    }

    function setPt(row, col) {
        var p = point(),
            lines = bufferLines(),
            lineLength;
        if (row < 0) {
            row = 0;
        }
        if (row > lines.length - 1) {
            row = lines.length - 1;
        }
        p.style.top = (fontHeight * row) + 'px';

        if (col < 0) {
            col = 0;
        }
        lineLength = lineLengthNoNewline(lines[row]);
        if (col > lineLength) {
            col = lineLength;
        }
        p.style.left = (fontWidth * col) + 'px';
    }

    function calculateFontSize() {
        var temp = document.createElement('span');
        temp.style.position = 'absolute';
        temp.textContent = ' ';
        selectedFrame().appendChild(temp);
        window.requestAnimationFrame(function () {
            var style = window.getComputedStyle(temp);
            fontWidth = parseFloat(style.width);
            fontHeight = parseFloat(style.height);
            temp.remove();
        });
    }

    function createStylesheetLink(href) {
        var link = document.createElement('link');
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = href;
        return link;
    }

    function loadTheme(theme) {
        var head = document.head;
        [].slice.call(head.querySelectorAll('link[rel=stylesheet]')).map(function (e) {
            e.remove();
        });
        head.appendChild(createStylesheetLink('layout.css'));
        head.appendChild(createStylesheetLink(theme));
        window.requestAnimationFrame(calculateFontSize);
    }

    // Adapted from http://stackoverflow.com/questions/6240139/highlight-text-range-using-javascript
    function getTextRange(element, start, end) {
        function getTextNodesIn(node) {
            if (node.nodeType === 3) {
                return [node];
            }
            return [].concat.apply([], [].slice.call(node.childNodes).map(getTextNodesIn));
        }
        var range = document.createRange(), textNodes = getTextNodesIn(element),
            foundStart = false, charCount = 0, i, textNode, endCharCount;
        for (i = 0; i < textNodes.length; i += 1) {
            textNode = textNodes[i];
            endCharCount = charCount + textNode.length;
            if (!foundStart && start >= charCount && (start < endCharCount ||
                    (start === endCharCount && i < textNodes.length))) {
                range.setStart(textNode, start - charCount);
                foundStart = true;
            }
            if (foundStart && end <= endCharCount) {
                range.setEnd(textNode, end - charCount);
                break;
            }
            charCount = endCharCount;
        }
        return range;
    }

    function floatStyle(element, style) {
        return parseFloat(element.style[style] || '0');
    }

    function ptRow(pt) {
        return Math.floor(floatStyle((pt || point()), 'top') / fontHeight);
    }

    function ptCol(pt) {
        return Math.floor(floatStyle((pt || point()), 'left') / fontWidth);
    }

    function ptOffset(pt) {
        return bufferLines().slice(0, ptRow(pt)).join('').length + ptCol(pt);
    }

    function updateRegion() {
        var marker = mark(),
            offset = ptOffset(),
            selection = window.getSelection(),
            markerOffset;
        if (marker) {
            selection.removeAllRanges();
            markerOffset = ptOffset(marker);
            if (markerOffset < offset) {
                selection.addRange(getTextRange(currentBuffer(), markerOffset, offset));
            } else {
                selection.addRange(getTextRange(currentBuffer(), offset, markerOffset));
            }
        }
    }

    function setPtUpdateRegion(row, col) {
        setPt(row, col);
        updateRegion();
    }

    function bufferUndoHistory(buffer) {
        if (!undoHistory[buffer.id]) {
            undoHistory[buffer.id] = [];
        }
        return undoHistory[buffer.id];
    }

    // Remote Editing API

    function gotoChar(offset) {
        var lines = bufferLines(offset),
            row = lines.length - 1;
        setPtUpdateRegion(row, lines[row].length);
    }

    function insertInternal(buffer, offset, text) {
        if (buffer.childElementCount === 0) {
            buffer.appendChild(document.createTextNode(''));
        }
        getTextRange(buffer, offset, offset).insertNode(document.createTextNode(text));
        buffer.normalize();
    }

    function deleteInternal(buffer, start, end) {
        var range = getTextRange(buffer, start, end),
            text = range.toString();
        range.deleteContents();
        buffer.normalize();
        return text;
    }

    // Commands, implemented in Clojure / Emacs Lisp

    function insertNoUndo(args) {
        var offset = ptOffset();
        insertInternal(currentBuffer(), offset, args);
        gotoChar(offset + args.length);
    }

    function insert(args) {
        var buffer = currentBuffer(),
            start = ptOffset(),
            end,
            history = bufferUndoHistory(buffer),
            lastUndo = history && history[history.length - 1];
        insertNoUndo(args);
        end = ptOffset();
        if (lastUndo && lastUndo.type === 'insert') {
            if (lastUndo.end >= start && lastUndo.start <= end) {
                start = Math.min(start, lastUndo.start);
                end = Math.max(end, lastUndo.end);
                history.pop();
            }
        }
        bufferUndoHistory(buffer).push({type: 'insert', start: start, end: end, text: args});
    }

    function deleteAndExtractRegionNoUndo(start, end) {
        var text = deleteInternal(currentBuffer(), start, end);
        gotoChar(start);
        return text;
    }

    function deleteRegion(start, end) {
        var buffer = currentBuffer(),
            text = deleteAndExtractRegionNoUndo(start, end);
        bufferUndoHistory(buffer).push({type: 'delete', start: start, end: end, text: text});
    }

    function overwriteMode() {
        currentBuffer().classList.toggle('overwrite-mode');
    }

    function backwardDeleteChar(n) {
        var end = ptOffset(),
            start = end - n;
        if (start >= 0) {
            deleteRegion(start, end);
        }
    }

    function deleteChar(n) {
        var start = ptOffset(),
            end = start + n;
        if (end >= 0) {
            deleteRegion(start, end);
        }
    }

    function backwardChar(n) {
        gotoChar(ptOffset() - n);
    }

    function forwardChar(n) {
        gotoChar(ptOffset() + n);
    }

    function previousLine(arg) {
        setPtUpdateRegion(ptRow() - arg,  ptCol());
    }

    function nextLine(arg) {
        setPtUpdateRegion(ptRow() + arg,  ptCol());
    }

    function backwardWord(arg) {
        var offset = ptOffset(),
            nextWord = currentBuffer().textContent.substring(0, offset).match(/\w*\s*$/g);
        if (nextWord) {
            gotoChar(offset - nextWord[0].length);
            if (arg > 1) {
                backwardWord(arg - 1);
            }
        }
    }

    function forwardWord(arg) {
        var offset = ptOffset(),
            nextWord = currentBuffer().textContent.substring(offset).match(/\s*\w*/g);
        if (nextWord) {
            gotoChar(offset + nextWord[0].length);
            if (arg > 1) {
                forwardWord(arg - 1);
            }
        }
    }

    function beginningOfLine(arg) {
        if (arg > 1) {
            nextLine(arg);
        }
        setPtUpdateRegion(ptRow(), 0);
    }

    function endOfLine(arg) {
        if (arg > 1) {
            nextLine(arg);
        }
        var row = ptRow();
        setPtUpdateRegion(row, lineLengthNoNewline(bufferLines()[row]));
    }

    function killLine(arg) {
        var row = ptRow(),
            col = ptCol(),
            offset = ptOffset(),
            lines = bufferLines(),
            line = lines[row],
            eol = lineLengthNoNewline(line);
        if (line.substring(col).trim() === '') {
            eol = line.length;
        }
        killRing = line.substring(col, eol);
        deleteRegion(offset, eol + offset - col);
        if (arg > 1) {
            killLine(arg - 1);
        }
    }

    function yank() {
        if (killRing) {
            insert(killRing);
        }
    }

    function undo() {
        var command = bufferUndoHistory(currentBuffer()).pop();
        if (command) {
            switch (command.type) {
            case 'insert':
                deleteAndExtractRegionNoUndo(command.start, command.end);
                break;
            case 'delete':
                gotoChar(command.start);
                insertNoUndo(command.text);
                break;
            }
        }
    }

    function selfInsertCommand(n) {
        var s = String.fromCharCode(n);
        if (matches(currentBuffer(), '.overwrite-mode')) {
            deleteChar(s.length);
        }
        insert(s);
    }

    function newline(arg) {
        arg = arg || 1;
        while (arg > 0) {
            selfInsertCommand(keys.newline);
            arg -= 1;
        }
    }

    function openLine(arg) {
        var offset = ptOffset();
        newline(arg);
        gotoChar(offset);
    }

    function setMark(pos) {
        pos = pos || ptOffset();
        var marker = mark(),
            selection = window.getSelection();
        if (marker) {
            selection.removeAllRanges();
            marker.remove();
        } else {
            marker = point().cloneNode(true);
            marker.classList.remove('point');
            marker.classList.add('mark');
            selectedWindow().appendChild(marker);
        }
    }

    // Window Management

    function createFrame() {
        var frame = document.createElement('div');
        frame.classList.add('frame');
        return frame;
    }

    function createMenuBar(menus) {
        var menubar = document.createElement('nav');
        menubar.innerHTML = menus.join(' ');
        menubar.classList.add('menu-bar');
        return menubar;
    }

    function createModeLine(line) {
        var modeLine = document.createElement('footer');
        modeLine.innerHTML = line + new Array(256).join('-');
        modeLine.classList.add('mode-line');
        return modeLine;
    }

    function createBuffer(id) {
        var buffer = document.createElement('div');
        buffer.id = id;
        buffer.classList.add('buffer');
        return buffer;
    }

    function createPoint() {
        var pt = document.createElement('span');
        pt.innerHTML = ' ';
        pt.classList.add('point');
        return pt;
    }

    function nextWindowId(frame) {
        return (frame || selectedFrame()).querySelectorAll('.window').length + 1;
    }

    function createWindow(id) {
        var window = document.createElement('div');
        window.id = id;
        window.classList.add('window');
        return window;
    }

    function createLiveWindow(id, buffer, modeLine) {
        var window = createWindow(id);
        window.classList.add('live');
        window.appendChild(buffer);
        window.appendChild(createPoint());
        if (modeLine) {
            window.appendChild(modeLine);
        }
        return window;
    }

    function createMinibufferWindow(id, buffer) {
        var window = createLiveWindow(id, buffer);
        buffer.classList.add('minibuffer-inactive-mode');
        window.classList.add('minibuffer');
        return window;
    }

    function selectWindow(window) {
        var selected = selectedWindow(),
            buffer = window.querySelector('.buffer'),
            current = currentBuffer();
        if (window === selected) {
            return window;
        }
        if (selected) {
            selected.classList.remove('selected');
            current.classList.remove('current');
            if (matches(selected, '.minibuffer')) {
                current.classList.add('minibuffer-inactive-mode');
            }
        }
        window.classList.add('selected');
        buffer.classList.add('current');
        if (matches(window, '.minibuffer')) {
            buffer.classList.remove('minibuffer-inactive-mode');
        }
        document.title = buffer.id;
        return window;
    }

    function siblingWindow(window) {
        return [].slice.call(window.parentElement.querySelectorAll('.window')).filter(function (w) {
            return w !== window;
        })[0];
    }

    function deleteWindow(window) {
        var parent = window.parentElement,
            grandparent = parent.parentElement,
            sibling = siblingWindow(window);
        if (grandparent && matches(parent, '.window')) {
            grandparent.replaceChild(sibling, parent);
            selectWindow(sibling);
        }
        return window;
    }

    function otherWindow(window) {
        return selectWindow(siblingWindow(window));
    }

    function splitWindow(frame, window, side) {
        var newWindow =  createWindow(nextWindowId(frame)),
            sibling = window.cloneNode(true),
            direction;

        if (side === 'right' || side === 'left') {
            direction = 'horizontally';
        } else {
            direction = 'vertically';
        }

        newWindow.classList.add('split-window', direction);
        window.parentElement.replaceChild(newWindow, window);
        newWindow.appendChild(window);

        sibling.id = nextWindowId(frame);
        sibling.classList.remove('selected');
        sibling.querySelector('.buffer').classList.remove('current');

        if (side === 'right' || side === 'below') {
            newWindow.appendChild(sibling);
        } else {
            newWindow.insertBefore(sibling, window);
        }

        return newWindow;
    }

    function initFrame() {
        var frame = createFrame(),
            buffer = createBuffer('*scratch*'),
            modeline = '-UUU:----F1  <strong>*scratch*</strong>      All L1     (Lisp Interaction) ',
            rootWindow = createLiveWindow(nextWindowId(frame), buffer, createModeLine(modeline));
        selectWindow(rootWindow);

        frame.appendChild(createMenuBar(['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help']));
        frame.appendChild(rootWindow);
        frame.appendChild(createMinibufferWindow(nextWindowId(frame), createBuffer(' *Minibuf-0*')));
        frame.classList.add('selected', 'menu-bar-mode', 'blink-cursor-mode', 'border');

        document.body.appendChild(frame);
        return frame;
    }

    // Keyboard, will send XTerm keys down, similar to how term.js works.

    function registerKeyboardHandler() {
        var keymap = {},
            keyUpTimeoutId;

        keymap[keys.ret] = newline;
        keymap[keys.backspace] = backwardDeleteChar;
        keymap[keys.del] = deleteChar;
        keymap[keys.ins] = overwriteMode;
        keymap[keys.left] = backwardChar;
        keymap[keys.up] = previousLine;
        keymap[keys.right] = forwardChar;
        keymap[keys.down] = nextLine;
        keymap[keys.ctrl] = {' ': setMark,
                             A: beginningOfLine,
                             B: backwardChar,
                             D: deleteChar,
                             E: endOfLine,
                             F: forwardChar,
                             J: newline,
                             K: killLine,
                             M: newline,
                             O: openLine,
                             P: previousLine,
                             N: nextLine,
                             V: yank,
                             Y: yank};
        keymap[keys.ctrl][String.fromCharCode(keys.slash)] = undo;
        keymap[keys.ctrl][String.fromCharCode(keys.left)] = backwardWord;
        keymap[keys.ctrl][String.fromCharCode(keys.right)] = forwardWord;

        window.addEventListener('keydown', function (e) {
            var prefixArg = 1,
                key = e.keyCode,
                command = keymap[key];
            if (!(key === keys.shift || key === keys.ctrl || key === keys.alt)) {
                clearTimeout(keyUpTimeoutId);
                selectedFrame().classList.add('keydown');
            }
            if (e.ctrlKey) {
                command = keymap[keys.ctrl][String.fromCharCode(key)];
            }
            if (command) {
                e.preventDefault();
                window.requestAnimationFrame(function () {
                    command(prefixArg);
                });
            }
        });

        ['keyup', 'blur'].forEach(function (e) {
            window.addEventListener(e, function () {
                keyUpTimeoutId = setTimeout(function () {
                    selectedFrame().classList.remove('keydown');
                }, 500);
            });
        });

        window.addEventListener('keypress', function (e) {
            e.preventDefault();
            window.requestAnimationFrame(function () {
                selfInsertCommand(e.charCode);
            });
        });
    }

    // Mouse

    function registerMouseHandler() {
        window.addEventListener('mousedown', function (e) {
            e.preventDefault();
            if (mouseButton.left === e.button
                     && matches(e.target, '.buffer:not(.minibuffer-inactive-mode')) {
                selectWindow(e.target.parentElement);
                var row = Math.floor(e.layerY / fontHeight),
                    col = Math.floor(e.layerX / fontWidth),
                    lines = bufferLines(),
                    line = lines[row],
                    offset = lines.slice(0, row).join('').length + Math.min(col, lineLengthNoNewline(line));
                window.requestAnimationFrame(function () {
                    gotoChar(offset);
                });
            }
        });

        window.oncontextmenu = function (e) {
            e.preventDefault();
        };
    }

    function unused() {
        return;
    }

    document.addEventListener('DOMContentLoaded', function () {
        initFrame();
        loadTheme('default-theme.css');
        registerKeyboardHandler();
        registerMouseHandler();

        unused(splitWindow, deleteWindow, otherWindow);
    });
}());

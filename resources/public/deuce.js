/*jslint browser: true */

// This is a spike for the Deuce web UI. In the real world it won't have as much logic on the client side.

// JS to XTerm, see keyPress and keyDown:
//  https://github.com/chjj/term.js/blob/master/src/term.js MIT
// Key handling pulled out:
//  https://github.com/Gottox/terminal.js/blob/master/lib/input/dom.js MIT/X

(function () {
    'use strict';

    var fontWidth, fontHeight,
        keys = {backspace: 8, newline: 10, ret: 13, left: 37, up: 38, right: 39, down: 40, del: 46},
        mouseButton = {left: 0, middle: 1, right: 2};

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

    function ptRow() {
        return Math.floor(floatStyle(point(), 'top') / fontHeight);
    }

    function ptCol() {
        return Math.floor(floatStyle(point(), 'left') / fontWidth);
    }

    function ptOffset() {
        return bufferLines().slice(0, ptRow()).join('').length + ptCol();
    }

    // Remote Editing API

    function gotoChar(offset) {
        var lines = bufferLines(offset),
            row = lines.length - 1;
        setPt(row, lines[row].length);
    }

    function insert(args) {
        var offset = ptOffset(),
            buffer = currentBuffer(),
            text = document.createTextNode(args);
        if (buffer.childElementCount === 0) {
            buffer.appendChild(document.createTextNode(''));
        }
        getTextRange(buffer, offset, offset).insertNode(text);
        gotoChar(offset + args.length);
    }

    function deleteRegion(start, end) {
        getTextRange(currentBuffer(), start, end).deleteContents();
        gotoChar(start);
    }

    // Commands, implemented in Clojure / Emacs Lisp

    function selfInsertCommand(n) {
        insert(String.fromCharCode(n));
    }

    function newline(arg) {
        arg = arg || 1;
        while (arg > 0) {
            selfInsertCommand(keys.newline);
            arg -= 1;
        }
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
        setPt(ptRow() - arg,  ptCol());
    }

    function nextLine(arg) {
        setPt(ptRow() + arg,  ptCol());
    }

    // Keyboard, will send XTerm keys down, similar to how term.js works.

    function registerKeyboardHandler() {
        var keymap = {};

        keymap[keys.ret] = newline;
        keymap[keys.backspace] = backwardDeleteChar;
        keymap[keys.del] = deleteChar;
        keymap[keys.left] = backwardChar;
        keymap[keys.up] = previousLine;
        keymap[keys.right] = forwardChar;
        keymap[keys.down] = nextLine;

        document.addEventListener('keydown', function (e) {
            var prefixArg = 1,
                command = keymap[e.keyCode];
            if (command) {
                e.preventDefault();
                window.requestAnimationFrame(function () {
                    command(prefixArg);
                });
            }
        });

        document.addEventListener('keypress', function (e) {
            e.preventDefault();
            window.requestAnimationFrame(function () {
                var prefixArg = 1,
                    command = keymap[e.charCode];
                if (command && e.charCode !== keys.del) {
                    command(prefixArg);
                } else {
                    selfInsertCommand(e.charCode);
                }
            });
        });
    }

    // Mouse

    function registerMouseHandler() {
        document.addEventListener('mousedown', function (e) {
            e.preventDefault();
            if (mouseButton.left === e.button && matches(e.target, '.buffer')) {
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

        document.oncontextmenu = function (e) {
            e.preventDefault();
        };
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
        if (selected) {
            selected.classList.remove('selected');
            current.classList.remove('current');
            if (matches(selected, 'minibuffer')) {
                current.classList.add('minibuffer-inactive-mode');
            }
        }
        window.classList.add('selected');
        buffer.classList.add('current');
        if (matches(window, 'minibuffer')) {
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
        if (grandparent && parent.classList.contains('window')) {
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

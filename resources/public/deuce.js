'use strict';
// This is a spike for the Deuce web UI. In the real world it won't have as much logic on the client side.

// JS to XTerm, see keyPress and keyDown:
//  https://github.com/chjj/term.js/blob/master/src/term.js MIT
// Key handling pulled out:
//  https://github.com/Gottox/terminal.js/blob/master/lib/input/dom.js MIT/X

(function () {
    var fontWidth,
        fontHeight,
        keys = {backspace: 8, newline: 10, ret: 13, left: 37, up: 38, right: 39, down: 40, del: 127},
        keymap = {};

    function selectedFrame() {
        return document.querySelector('.frame');
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

    function bufferText() {
        var buffer = currentBuffer(),
            text = buffer.childNodes[0];
        if (!text) {
            text = document.createTextNode('');
            buffer.appendChild(text);
        }
        return text;
    }

    function bufferLines(offset) {
        var linesAndBreaks = bufferString().substring(0, offset).split(/(\r?\n)/),
            lines = [],
            i;

        for (i = 0; i < linesAndBreaks.length; i += 2) {
            lines.push(linesAndBreaks[i] + (linesAndBreaks[i + 1] || ""));
        }
        return lines;
    }

    function point() {
        return currentBuffer().parentElement.querySelector('.point');
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
        lineLength = (lines[row] || "").replace(/(\r?\n)*$/, '').length;
        if (col > lineLength) {
            col = lineLength;
        }
        p.style.left = (fontWidth * col) + 'px';
    }

    function setPtOffset(offset) {
        var lines = bufferLines(offset),
            row = lines.length - 1;
        setPt(row, lines[row].length);
    }
    function floatStyle(e, p) {
        return parseFloat(e.style[p] || '0');
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

    function selfInsertCommand(n) {
        var offset = ptOffset();
        bufferText().insertData(offset, String.fromCharCode(n));
        setPtOffset(offset + 1);
    }

    function newline(arg) {
        arg = arg || 1;
        while (arg > 0) {
            selfInsertCommand(keys.newline);
            arg -= 1;
        }
    }

    function backwardDeleteChar(n) {
        var offset = ptOffset() - n;
        if (offset >= 0) {
            bufferText().deleteData(offset, n);
            setPtOffset(offset);
        }
    }

    function deleteChar(n) {
        var offset = ptOffset();
        if (offset >= 0) {
            bufferText().deleteData(offset, n);
        }
    }

    function backwardChar(n) {
        setPtOffset(ptOffset() - n);
    }

    function forwardChar(n) {
        setPtOffset(ptOffset() + n);
    }

    function previousLine(arg) {
        setPt(ptRow() - arg,  ptCol());
    }

    function nextLine(arg) {
        setPt(ptRow() + arg,  ptCol());
    }

    keymap[keys.ret] = newline;
    keymap[keys.backspace] = backwardDeleteChar;
    keymap[keys.del] = deleteChar;
    keymap[keys.left] = backwardChar;
    keymap[keys.up] = previousLine;
    keymap[keys.right] = forwardChar;
    keymap[keys.down] = nextLine;

    function registerKeyboardHandler() {
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
                    command = keymap[e.keyCode];
                if (command) {
                    command(prefixArg);
                } else {
                    selfInsertCommand(e.charCode);
                }
            });
        });
    }

    function calculateFontSize() {
        var temp = document.createElement('span');
        temp.style.position = 'absolute';
        temp.textContent = ' ';
        selectedWindow().appendChild(temp);
        window.requestAnimationFrame(function () {
            var style = window.getComputedStyle(temp);
            fontWidth = parseFloat(style.width);
            fontHeight = parseFloat(style.height);
            temp.remove();
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        calculateFontSize();
        registerKeyboardHandler();
    });
}());

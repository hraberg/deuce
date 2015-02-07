// This is a spike for the Deuce web UI. In the real world it won't have as much logic on the client side.

// JS to XTerm, see keyPress and keyDown:
//  https://github.com/chjj/term.js/blob/master/src/term.js MIT
// Key handling pulled out:
//  https://github.com/Gottox/terminal.js/blob/master/lib/input/dom.js MIT/X

(function () {
    'use strict';

    var fontWidth,
        fontHeight,
        keys = {backspace: 8, newline: 10, ret: 13, left: 37, up: 38, right: 39, down: 40, del: 127},
        keymap = {};

    function selectedWindow() {
        return document.querySelector('.selected.window');
    }

    function currentBuffer() {
        return document.querySelector('.current.buffer');
    }

    function bufferString() {
        return currentBuffer().textContent;
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

    function selfInsertCommand(n) {
        var offset = ptOffset(),
            range = getTextRange(currentBuffer(), offset, offset);
        range.insertNode(document.createTextNode(String.fromCharCode(n)));
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
        var end = ptOffset(),
            start = end - n;
        if (start >= 0) {
            getTextRange(currentBuffer(), start, end).deleteContents();
            setPtOffset(start);
        }
    }

    function deleteChar(n) {
        var start = ptOffset(),
            end = start + n;
        if (end >= 0) {
            getTextRange(currentBuffer(), start, end).deleteContents();
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

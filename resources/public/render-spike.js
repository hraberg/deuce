/*jslint browser: true */

document.addEventListener('DOMContentLoaded', function () {
    'use strict';

    function bufferLines(s) {
        var linesAndBreaks = s.split(/(\r?\n)/),
            lines = [],
            i;

        for (i = 0; i < linesAndBreaks.length; i += 2) {
            lines.push(linesAndBreaks[i] + (linesAndBreaks[i + 1] || ''));
        }
        return lines;
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

    function deleteInternal(buffer, start, end) {
        var range = getTextRange(buffer, start, end),
            text = range.toString();
        range.deleteContents();
        return text;
    }

    var buffer = document.querySelector('#buffer'),
        win = document.querySelector('#window'),
        display = document.querySelector('#display'),
        pendingRedraw = false,
        fontHeight,
        fontWidth,
        width = 132,
        height = 43,
        file = new Array(1000).join(document.querySelector('#tutorial').textContent + '\n'),
        linesInFile = bufferLines(file),
        visibleText = '',
        visibleStart = 0,
        visibleEnd = 0;


    console.log('lines: ' + linesInFile.length, (Math.round(file.length / (1024 * 1024) * 100) / 100) + 'Mb');
    function calculateFontSize() {
        var temp = document.createElement('span');
        temp.style.position = 'absolute';
        temp.textContent = ' ';
        buffer.appendChild(temp);
        window.requestAnimationFrame(function () {
            var style = window.getComputedStyle(temp);
            fontWidth = parseFloat(style.width);
            fontHeight = parseFloat(style.height);
            temp.remove();
            buffer.style.height = (fontHeight * linesInFile.length) + 'px';

            win.style.width = (width * fontWidth) + 'px';
            win.style.height = (height * fontHeight) + 'px';

            win.onscroll();
            win.focus();
        });
    }

    win.onscroll = function () {
        if (!pendingRedraw) {
            pendingRedraw = true;
            window.requestAnimationFrame(function () {
                var t = Date.now(),
                    line = Math.floor(win.scrollTop / fontHeight),
                    newStart = line,
                    newEnd = line + height,
                    newVisibleText = linesInFile.slice(newStart, newEnd).join(''),
                    diff = newStart - visibleStart,
                    insertAtScrollDown,
                    useDeltas = true;
                console.log('line: ' + line);
                if (useDeltas && Math.abs(diff) < height && diff !== 0) {
                    console.log('diff: ' + diff);
                    if (diff > 0) {
                        deleteInternal(display, 0, linesInFile.slice(visibleStart, newStart).join('').length);
                        display.appendChild(document.createTextNode(linesInFile.slice(visibleEnd, newEnd).join('')));
                    } else {
                        deleteInternal(display,
                                       visibleText.length - linesInFile.slice(newEnd, visibleEnd).join('').length,
                                       visibleText.length);
                        insertAtScrollDown = document.createTextNode(linesInFile.slice(newStart, visibleStart).join(''));
                        if (display.firstChild) {
                            display.insertBefore(insertAtScrollDown, display.firstChild);
                        } else {
                            display.appendChild(insertAtScrollDown);
                        }
                    }
                    display.normalize();
                } else {
                    console.log('full redraw');
                    display.innerHTML = newVisibleText;
                }

                visibleStart = newStart;
                visibleEnd = newEnd;
                visibleText = newVisibleText;

                pendingRedraw = false;
                console.log((Date.now() - t) + 'ms');
            });
        }
    };

    calculateFontSize();
});

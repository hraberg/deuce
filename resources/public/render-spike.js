/*jslint browser: true regexp: true */

document.addEventListener('DOMContentLoaded', function () {
    'use strict';

    function bufferLines(s) {
        return s.match(/^.*((\r\n|\n|\r)|$)/gm);
    }

    var scrollBuffer = document.querySelector('.scroll-buffer'),
        win = document.querySelector('.window'),
        display = document.querySelector('.display'),
        clipboard = document.querySelector('.clipboard'),
        point = document.querySelector('.point'),
        frame = document.querySelector('.frame'),
        pendingRedraw = false,
        requestScroll = false,
        fontHeight,
        fontWidth,
        gutterWidth = 0,
        gutterVisible = false,
        width = 132,
        height = 38,
        file = new Array(1000).join(document.querySelector('[data-filename=TUTORIAL]').textContent + '\n'),
        linesInFile = bufferLines(file),
        offset = 0,
        currentLine = 0,
        visibleStart = 0,
        newVisibleStart = 0,
        prefixArg = 1,
        keys = {left: 37,  up: 38, right: 39, down: 40},
        keymap,
        keyUpTimeoutId;

    function offsetOfLine(idx) {
        var i, acc = 0;
        for (i = 0; i < idx; i += 1) {
            acc += linesInFile[i].length;
        }
        return acc;
    }

    function lineAtOffset(offset) {
        var i, acc = 0, lines = linesInFile.length;
        for (i = 0; i < lines; i += 1) {
            acc += linesInFile[i].length;
            if (offset < acc) {
                return i;
            }
        }
        return -1;
    }

    function normalizeSelector(selector) {
        return selector && selector.trim().replace('::', ':');
    }

    function setCssRule(selector, css) {
        var styleSheet = document.styleSheets[0],
            i;
        for (i = 0; i < styleSheet.cssRules.length; i += 1) {
            if (normalizeSelector(styleSheet.cssRules[i].selectorText) === selector) {
                styleSheet.deleteRule(i);
                break;
            }
        }
        styleSheet.insertRule(selector + ' ' + css, 0);
    }

    function alignLineNumbers() {
        gutterWidth = ((linesInFile.length.toString().length + 1) * fontWidth);
        setCssRule('*.window.linum-mode', '{ padding-left: ' + gutterWidth + 'px; }');
        setCssRule('*.window.linum-mode .line:before', '{ width: ' + gutterWidth + 'px; margin-left: ' + -gutterWidth + 'px; }');
    }

    function alignDisplay() {
        display.style.top = (win.offsetTop + win.clientTop) + 'px';
        display.style.left = (win.offsetLeft + win.clentLeft) + 'px';
    }

    function calculateFontSize() {
        var temp = document.createElement('span');
        temp.style.position = 'absolute';
        temp.textContent = ' ';
        display.appendChild(temp);
        function setCalculatedFontSize() {
            var style = window.getComputedStyle(temp),
                parsedWidth = parseFloat(style.width),
                parsedHeight = parseFloat(style.height);
            if (!(parsedWidth && parsedHeight)) {
                return window.requestAnimationFrame(setCalculatedFontSize);
            }
            fontWidth = parsedWidth;
            fontHeight = parsedHeight;
            temp.remove();

            scrollBuffer.style.height = (fontHeight * linesInFile.length) + 'px';
            win.style.width = (width * fontWidth) + 'px';
            win.style.height = (height * fontHeight) + 'px';

            point.style.width = fontWidth + 'px';
            point.style.height = fontHeight + 'px';
        }
        window.requestAnimationFrame(setCalculatedFontSize);
    }

    function renderLine(idx) {
        var line = document.createElement('span');
        line.dataset.line = idx + 1;
        line.classList.add('line');
        line.innerHTML = (linesInFile[idx] || '').replace('<', '&lt;');
        return line;
    }

    function renderPoint(startLine) {
        var lineOffset = offsetOfLine(currentLine),
            row = currentLine - startLine,
            col = offset - lineOffset,
            startLinePx = startLine * fontHeight;

        console.log('window start line:', startLine, 'line:', currentLine, 'offset:', offset, 'row:', row, 'col:', col);
        point.style.left = (col * fontWidth + (gutterVisible ? gutterWidth : 0)) + 'px';
        point.style.top = (row * fontHeight) + 'px';

        if (requestScroll && startLine !== visibleStart) {
            win.scrollTop = startLinePx;
            requestScroll = false;
        }
    }

    function render() {
        var t = Date.now(),
            newStart = newVisibleStart,
            newEnd = newStart + height,
            diff = newStart - visibleStart,
            fragment = document.createDocumentFragment(),
            useDeltas = true,
            i;

        renderPoint(newStart);

        if (useDeltas && Math.abs(diff) < height && diff !== 0) {
            console.log('diff:', diff);
            if (diff > 0) {
                for (i = diff; i > 0; i -= 1) {
                    display.firstChild.remove();
                    fragment.appendChild(renderLine(newEnd - i));
                }
                display.appendChild(fragment);
            } else {
                for (i = 0; i < -diff; i += 1) {
                    display.lastChild.remove();
                    fragment.appendChild(renderLine(newStart + i));
                }
                display.insertBefore(fragment, display.firstChild);
            }
        } else {
            console.log('full redraw');
            for (i = newStart; i < newEnd; i += 1) {
                fragment.appendChild(renderLine(i));
            }
            display.innerHTML = '';
            display.appendChild(fragment);
        }

        visibleStart = newStart;
        pendingRedraw = false;

        console.log((Date.now() - t) + 'ms');
    }

    function requestRedraw(scroll) {
        requestScroll = scroll || requestScroll;
        if (!pendingRedraw) {
            pendingRedraw = true;
            window.requestAnimationFrame(render);
        }
    }

    function limit(x, min, max) {
        return Math.max(min, Math.min(x, max));
    }

    function bufferSize() {
        return offsetOfLine(linesInFile.length);
    }

    function gotoChar(n) {
        offset = limit(n, 0, bufferSize());
        currentLine = lineAtOffset(offset);
        if (visibleStart > currentLine || ((visibleStart + height) < currentLine + 1)) {
            newVisibleStart = limit(Math.floor(currentLine - height / 2), 0, linesInFile.length);
            requestRedraw(true);
        } else {
            requestRedraw(false);
        }
    }

    function forwardChar(n) {
        gotoChar(offset + n);
    }

    function backwardChar(n) {
        forwardChar(-n);
    }

    function nextLine(arg) {
        var col = offset - offsetOfLine(currentLine),
            newLine =  limit(currentLine + arg, 0, linesInFile.length - 1),
            newLineOffset = offsetOfLine(newLine),
            newOffset = newLineOffset + Math.min(col, (linesInFile[newLine] || '').length);
        gotoChar(newOffset);
    }

    function previousLine(arg) {
        nextLine(-arg);
    }

    keymap = {};
    keymap[keys.up] = previousLine;
    keymap[keys.down] = nextLine;
    keymap[keys.left] = backwardChar;
    keymap[keys.right] = forwardChar;

    window.addEventListener('keydown', function (e) {
        var key = e.keyCode,
            command = keymap[key];
        if (!(key === keys.shift || key === keys.ctrl || key === keys.alt)) {
            clearTimeout(keyUpTimeoutId);
            frame.classList.add('keydown');
        }
        if (command) {
            e.preventDefault();
            command(prefixArg);
        }
    });

    ['keyup', 'blur'].forEach(function (e) {
        window.addEventListener(e, function () {
            keyUpTimeoutId = setTimeout(function () {
                frame.classList.remove('keydown');
            }, 500);
        });
    });


    win.addEventListener('scroll', function () {
        var newOffset, newLine;
        newVisibleStart = Math.floor(win.scrollTop / fontHeight);
        newLine = newVisibleStart;
        console.log(newLine, currentLine, offset);
        if (newLine === 0) {
            console.log('at top');
            newOffset = 0;
            newLine = 0;
        } else if (newLine === (linesInFile.length - 1)) {
            console.log('at bottom');
            newLine = linesInFile.length - 1;
            newOffset = offsetOfLine(newLine) + linesInFile[newLine].length;
        } else if (newLine > currentLine) {
            console.log('scrolling down');
            newOffset = offsetOfLine(newLine);
        } else {
            console.log('scrolling up');
            newLine = newLine + height - 2;
            newOffset = offsetOfLine(newLine);
        }
        offset = newOffset;
        currentLine = newLine;
        requestRedraw(false);
    });

    document.querySelector('[name=linum-mode]').addEventListener('click', function (e) {
        gutterVisible =  win.classList.toggle(e.target.name);
        requestRedraw(false);
    });

    function resize() {
        calculateFontSize();
        window.requestAnimationFrame(function () {
            alignLineNumbers();
            alignDisplay();
            render();
            win.focus();
        });
    }

    window.addEventListener('resize', resize);
    resize();

    window.addEventListener('paste', function (e) {
        e.preventDefault();
        console.log('paste', e.clipboardData.getData('text/plain'));
    });

    function handleCopyAndCut(type, text) {
        console.log(type, text);
        clipboard.value = text;
        clipboard.select();
        setTimeout(function () {
            clipboard.blur();
            clipboard.value = '';
        });
    }

    window.addEventListener('cut', function () {
        handleCopyAndCut('cut', 'Cut from Deuce' + new Date());
    });

    window.addEventListener('copy', function () {
        handleCopyAndCut('copy', 'Copied from Deuce ' + new Date());
    });

    console.log('lines:', linesInFile.length, (Math.round(file.length / (1024 * 1024) * 100) / 100), 'Mb');
});

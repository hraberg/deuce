/*jslint browser: true regexp: true */

document.addEventListener('DOMContentLoaded', function () {
    'use strict';

    function bufferLines(s) {
        return s.match(/^.*((\r\n|\n|\r)|$)/gm);
    }

    var DEBUG = true,
        scrollBuffer = document.querySelector('.scroll-buffer'),
        win = document.querySelector('.window'),
        display = document.querySelector('.display'),
        clipboard = document.querySelector('.clipboard'),
        point = document.querySelector('.point'),
        frame = document.querySelector('.frame'),
        pendingRedraw = false,
        requestScroll = false,
        forceRedraw = false,
        fontHeight,
        fontWidth,
        gutterWidth = 0,
        gutterVisible = false,
        width = 132,
        height = 38,
        tabWidth = 8,
        file = new Array(1000).join(document.querySelector('[data-filename=TUTORIAL]').textContent + '\n'),
        linesInFile = bufferLines(file),
        lineOffsets = {},
        offset = 0,
        currentLine = 0,
        currentCol = 0,
        desiredVisibleCol = 0,
        visibleStart = 0,
        newVisibleStart = 0,
        prefixArg = 1,
        keys = {left: 37,  up: 38, right: 39, down: 40},
        mouseButton = {left: 0},
        keymap,
        keyUpTimeoutId;

    function debug() {
        if (DEBUG) {
            console.log.apply(console, [].slice.call(arguments));
        }
    }

    function offsetOfLine(idx) {
        var i, acc = 0, cache = lineOffsets[idx];
        if (cache) {
            return cache;
        }
        for (i = 0; i < idx; i += 1) {
            lineOffsets[i] = acc;
            acc += (linesInFile[i] || '').length;
        }
        lineOffsets[idx] = acc;
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
        return lines - 1;
    }

    function limit(x, min, max) {
        return Math.max(min, Math.min(x, max));
    }

    function lineColumn(line, col) {
        return limit(col, 0, (linesInFile[line] || '').length - 1);
    }

    function lineVisibleColumn(line, col) {
        var i, visibleCol = 0;
        line = linesInFile[line];
        for (i = 0; i < col; i += 1) {
            visibleCol += (line[i] === '\t' ? tabWidth : 1);
        }
        return visibleCol;
    }

    function visibleColumnToColumn(line, visibleCol) {
        var i, col = 0;
        line = linesInFile[line];
        for (i = 0; i < line.length && col < visibleCol; i += 1) {
            col += (line[i] === '\t' ? tabWidth : 1);
        }
        if (limit(col, 0, visibleCol) % tabWidth !== 0 && line[i - 1] === '\t') {
            return i - 1;
        }
        return i;
    }

    function bufferSize() {
        return offsetOfLine(linesInFile.length);
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
        line.innerHTML = (linesInFile[idx] || '').replace(/</g, '&lt;');
        return line;
    }

    function renderPoint(startLine) {
        var row = currentLine - startLine,
            visibleCol = lineVisibleColumn(currentLine, currentCol),
            startLinePx = startLine * fontHeight;

        debug('point:', 'visible start line:', startLine,  'visible row:', row,
              'visible col:', visibleCol, 'desired visible col:', desiredVisibleCol);
        point.style.left = (visibleCol * fontWidth + (gutterVisible ? gutterWidth : 0)) + 'px';
        point.style.top = (row * fontHeight) + 'px';

        if (requestScroll && startLine !== visibleStart) {
            win.scrollTop = startLinePx;
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

        if (useDeltas && !forceRedraw && Math.abs(diff) < height && diff !== 0) {
            debug('diff redraw:', diff);
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
        } else if (diff !== 0 || forceRedraw) {
            debug('full redraw:', diff);
            for (i = newStart; i < newEnd; i += 1) {
                fragment.appendChild(renderLine(i));
            }
            display.innerHTML = '';
            display.appendChild(fragment);
            forceRedraw = false;
        }

        visibleStart = newStart;
        pendingRedraw = false;

        debug('render:', (Date.now() - t), 'ms');
    }

    function requestRedraw(scroll, force) {
        requestScroll = scroll || requestScroll;
        forceRedraw = force || forceRedraw;
        if (!pendingRedraw) {
            pendingRedraw = true;
            window.requestAnimationFrame(render);
        }
    }

    function gotoChar(n, line, keepDesiredVisibleColumn) {
        offset = limit(n, 0, bufferSize());
        currentLine = line || lineAtOffset(offset);
        currentCol = offset - offsetOfLine(currentLine);
        debug('goto char:', 'offset:', offset, 'line:', currentLine, 'col:', currentCol);
        if (!keepDesiredVisibleColumn) {
            desiredVisibleCol = lineVisibleColumn(currentLine, currentCol);
        }
        if (visibleStart > currentLine || ((visibleStart + height) < currentLine + 1)) {
            newVisibleStart = limit(Math.floor(currentLine - height / 2), 0, linesInFile.length);
            requestRedraw(true, false);
        } else {
            requestRedraw(false, false);
        }
    }

    function forwardChar(n) {
        var newOffset = offset + n,
            line = (linesInFile[currentLine] || ''),
            lineOffset = offsetOfLine(currentLine),
            isSameLine = newOffset >= lineOffset && newOffset < lineOffset + line.length;
        gotoChar(newOffset, isSameLine ? currentLine : null);
    }

    function backwardChar(n) {
        forwardChar(-n);
    }

    function nextLine(arg) {
        var newLine = limit(currentLine + arg, 0, linesInFile.length - 1),
            newLineOffset = offsetOfLine(newLine),
            newLineVisibleLength = lineVisibleColumn(newLine, linesInFile[newLine].length - 1),
            visibleCol = desiredVisibleCol < newLineVisibleLength ? desiredVisibleCol : lineVisibleColumn(currentLine, currentCol),
            newCol = lineColumn(newLine, visibleColumnToColumn(newLine, visibleCol)),
            newOffset = newLineOffset + newCol;
        gotoChar(newOffset, newLine, true);
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
            command = keymap[key],
            t;
        if (!(key === keys.shift || key === keys.ctrl || key === keys.alt)) {
            if (keyUpTimeoutId) {
                clearTimeout(keyUpTimeoutId);
                keyUpTimeoutId = null;
            }
            frame.classList.add('keydown');
        }
        if (command) {
            e.preventDefault();
            t = Date.now();
            command(prefixArg);
            debug('command:', command.name, Date.now() - t, 'ms');
        }
    });

    ['keyup', 'blur'].forEach(function (e) {
        window.addEventListener(e, function () {
            if (!keyUpTimeoutId) {
                keyUpTimeoutId = setTimeout(function () {
                    frame.classList.remove('keydown');
                    keyUpTimeoutId = null;
                }, 500);
            }
        });
    });

    win.addEventListener('mousedown', function (e) {
        if (mouseButton.left === e.button) {
            e.preventDefault();
            var rect = win.getBoundingClientRect(),
                x = e.clientX - rect.left,
                y = e.clientY - rect.top,
                row = Math.floor(y / fontHeight),
                col = Math.floor(x / fontWidth),
                line = row + visibleStart,
                realCol = lineColumn(line, visibleColumnToColumn(line, col));
            debug('mouse click:', 'x:', x, 'y:', y, 'visible col:', col, 'visible row:', row);
            gotoChar(offsetOfLine(line) + realCol, line);
            win.focus();
        }
    });

    win.addEventListener('scroll', function () {
        if (requestScroll) {
            requestScroll = false;
            return;
        }
        var newOffset, newLine;
        newVisibleStart = Math.floor(win.scrollTop / fontHeight);
        newLine = newVisibleStart;
        debug('scrolling:', 'new visible start line:', newLine, 'line:', currentLine, 'offset:', offset);
        if (newLine === 0) {
            debug('at top');
            newOffset = 0;
            newLine = 0;
        } else if (newLine === (linesInFile.length - 1)) {
            debug('at bottom');
            newLine = linesInFile.length - 1;
            newOffset = offsetOfLine(newLine) + linesInFile[newLine].length;
        } else if (newLine > currentLine) {
            debug('scrolling down');
            newOffset = offsetOfLine(newLine);
        } else {
            debug('scrolling up');
            newLine = newLine + height - 2;
            newOffset = offsetOfLine(newLine);
        }
        offset = newOffset;
        currentLine = newLine;
        requestRedraw(false, false);
    });

    document.querySelector('[name=linum-mode]').addEventListener('click', function (e) {
        gutterVisible =  win.classList.toggle(e.target.name);
        win.focus();
        requestRedraw(false, false);
    });

    function resize() {
        calculateFontSize();
        window.requestAnimationFrame(function () {
            alignLineNumbers();
            alignDisplay();
            requestRedraw(true, true);
            win.focus();
        });
    }

    window.addEventListener('resize', resize);
    resize();

    window.addEventListener('paste', function (e) {
        e.preventDefault();
        debug('paste:', e.clipboardData.getData('text/plain'));
    });

    function handleCopyAndCut(type, text) {
        debug(type + ':', text);
        clipboard.value = text;
        clipboard.select();
        setTimeout(function () {
            clipboard.blur();
            clipboard.value = '';
        });
    }

    window.addEventListener('cut', function () {
        handleCopyAndCut('cut', linesInFile[currentLine]);
    });

    window.addEventListener('copy', function () {
        handleCopyAndCut('copy', linesInFile[currentLine]);
    });

    debug('lines:', linesInFile.length, (Math.round(file.length / (1024 * 1024) * 100) / 100), 'Mb');
});

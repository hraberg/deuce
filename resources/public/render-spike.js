/*jslint browser: true regexp: true */

document.addEventListener('DOMContentLoaded', function () {
    'use strict';

    function bufferLines(s) {
        return s.match(/^.*((\r\n|\n|\r)|$)/gm);
    }

    var scrollBuffer = document.querySelector('.scroll-buffer'),
        win = document.querySelector('.window'),
        display = document.querySelector('.display'),
        pendingRedraw = false,
        fontHeight,
        fontWidth,
        width = 132,
        height = 43,
        file = new Array(1000).join(document.querySelector('#tutorial').textContent + '\n'),
        linesInFile = bufferLines(file),
        visibleStart = 0;

    function calculateFontSize() {
        var temp = document.createElement('span');
        temp.style.position = 'absolute';
        temp.textContent = ' ';
        win.appendChild(temp);
        window.requestAnimationFrame(function () {
            var style = window.getComputedStyle(temp);
            fontWidth = parseFloat(style.width);
            fontHeight = parseFloat(style.height);
            temp.remove();

            scrollBuffer.style.height = (fontHeight * linesInFile.length) + 'px';
            win.style.width = (width * fontWidth) + 'px';
            win.style.height = (height * fontHeight) + 'px';
        });
    }

    function render() {
        var t = Date.now(),
            newStart = Math.floor(win.scrollTop / fontHeight),
            newEnd = newStart + height,
            diff = newStart - visibleStart,
            fragment = document.createDocumentFragment(),
            useDeltas = true,
            i;

        console.log('line: ' + newStart);

        if (useDeltas && Math.abs(diff) < height && diff !== 0) {
            console.log('diff: ' + diff);
            if (diff > 0) {
                for (i = diff; i > 0; i -= 1) {
                    display.firstChild.remove();
                    fragment.appendChild(document.createTextNode(linesInFile[newEnd - i]));
                }
                display.appendChild(fragment);
            } else {
                for (i = 0; i < -diff; i += 1) {
                    display.lastChild.remove();
                    fragment.appendChild(document.createTextNode(linesInFile[newStart + i]));
                }
                display.insertBefore(fragment, display.firstChild);
            }
        } else {
            console.log('full redraw');
            for (i = newStart; i < newEnd; i += 1) {
                fragment.appendChild(document.createTextNode(linesInFile[i]));
            }
            display.innerHTML = '';
            display.appendChild(fragment);
        }

        visibleStart = newStart;
        pendingRedraw = false;

        console.log((Date.now() - t) + 'ms');
    }

    win.addEventListener('scroll', function () {
        if (!pendingRedraw) {
            pendingRedraw = true;
            window.requestAnimationFrame(render);
        }
    });

    calculateFontSize();
    window.requestAnimationFrame(render);

    console.log('lines: ' + linesInFile.length, (Math.round(file.length / (1024 * 1024) * 100) / 100) + 'Mb');
});

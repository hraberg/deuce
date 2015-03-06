/*eslint-env browser */

'use strict';

let DeuceElement = Object.create(HTMLElement.prototype);

DeuceElement.createdCallback = () => {
    let template = document.querySelector('template[data-tag=' + this.tagName.toLowerCase() + ']').content,
        clone = document.importNode(template, true);
    this.createShadowRoot().appendChild(clone);
};

DeuceElement.attachedCallback = () => {
    if (this.attributeChangedCallback) {
        let element = this;
        setTimeout(() => [].slice.call(element.attributes).forEach((attr) => {
            let value = attr.value;
            element.attributeChangedCallback(attr.name, value, value);
        }));
    }
};

DeuceElement.resize = () => {
    let point = this.querySelector('::shadow point-d'),
        rect = this.getBoundingClientRect();
    this.setAttribute('width', Math.round(rect.width / point.charWidth));
    this.setAttribute('height', Math.round(rect.height / point.charHeight));
};

DeuceElement.intAttribute = (name) => parseInt(this.getAttribute(name), 10);

let DeucePoint = Object.create(DeuceElement);

DeucePoint.attachedCallback = () => {
    this.point = this.querySelector('::shadow span');
    let rect = (this.point.querySelector('.undecorated-point') || this.point).getBoundingClientRect();
    this.charWidth = parseFloat(rect.width);
    this.charHeight = parseFloat(rect.height);
    DeuceElement.attachedCallback.call(this);
};

DeucePoint.moveTo = (column, visibleLine) => {
    if (![column, visibleLine, this.charWidth, this.charHeight].some(Number.isNaN)) {
        let x = (this.charWidth * column),
            y = (this.charHeight) * visibleLine,
            transform = 'translate3d(' + x + 'px, ' + y + 'px, 0)';
        this.point.style.transform = transform;
    }
};

let DeuceMark = Object.create(DeucePoint);

DeuceMark.attachedCallback = () => {
    this.start = this.querySelector('::shadow .start');
    this.mid = this.querySelector('::shadow .mid');
    this.end = this.querySelector('::shadow .end');
    this.markRegion = this.querySelector('::shadow .mark-region');
    DeucePoint.attachedCallback.call(this);
};

DeuceMark.update = (startColumn, startLine, endColumn, endLine) => {
    if (![startLine, startColumn, endLine, endColumn].some(Number.isNaN)) {
        if (startLine === endLine) {
            let startX = this.charWidth * startColumn,
                startY = this.charHeight * startLine,
                startTransform = 'translate3d(' + startX + 'px, ' + startY + 'px, 0)';

            this.markRegion.classList.add('same-line');

            this.start.style.transform = startTransform;
            this.start.style.height = this.charHeight + 'px';
            this.start.style.width = (endColumn - startColumn) * this.charWidth + 'px';
        } else {
            let startX = this.charWidth * startColumn,
                startY = this.charHeight * startLine,
                startTransform = 'translate3d(' + startX + 'px, ' + startY + 'px, 0)',
                endX = 0,
                endY = this.charHeight * (startLine + 1),
                endTransform = 'translate3d(' + endX + 'px, ' + endY + 'px, 0)',
                windowWidth = window.innerWidth + 'px';

            this.markRegion.classList.remove('same-line');

            this.start.style.transform = startTransform;
            this.start.style.height = ((endLine - startLine) * this.charHeight) + 'px';
            this.start.style.width = windowWidth;

            this.mid.style.transform = endTransform;
            this.mid.style.height = ((endLine - startLine - 1) * this.charHeight) + 'px';
            this.mid.style.width = windowWidth;

            this.end.style.transform = endTransform;
            this.end.style.height = ((endLine - startLine) * this.charHeight) + 'px';
            this.end.style.width = (endColumn * this.charWidth) + 'px';
        }
    }
};

let DeuceBuffer = Object.create(DeuceElement);

DeuceBuffer.attachedCallback = () => {
    this.point = this.querySelector('::shadow point-d');
    this.mark = this.querySelector('::shadow mark-d');
    this.scrollBuffer = this.querySelector('::shadow .scroll-buffer');
    this.scrollPane = this.querySelector('::shadow .scroll-pane');
    this.display = this.querySelector('::shadow .display');

    this.win = this.parentElement;
    this.resize = this.resize.bind(this);
    window.addEventListener('resize', this.resize);
    setTimeout(this.resize);
    DeuceElement.attachedCallback.call(this);
};

DeuceBuffer.detachedCallback = () => {
    window.removeEventListener('resize', this.resize);
};

DeuceBuffer.resize = () => {
    DeuceElement.resize.call(this);
    this.scrollPane.style.height = this.display.getBoundingClientRect().height + 'px';
};

DeuceBuffer.attributeChangedCallback = (attrName) => {
    if (!this.parentElement) {
        return;
    }
    if (attrName === 'current-column' || attrName === 'line-number-at-point') {
        let column = this.intAttribute('current-column'),
            firstLineInClientBuffer = this.querySelector('line-d').intAttribute('number'),
            lineNumberAtPoint = this.intAttribute('line-number-at-point'),
            lineNumberAtStart = this.win.intAttribute('line-number-at-start'),
            visibleLine = (lineNumberAtPoint - (firstLineInClientBuffer - lineNumberAtStart)) - lineNumberAtStart;
        this.point.moveTo(column, visibleLine);
        this.updateMark();
    }
    if (attrName === 'line-number-at-point-max') {
        let lineNumberAtPointMax = this.intAttribute('line-number-at-point-max');
        this.scrollBuffer.style.height = (lineNumberAtPointMax * this.point.charHeight) + 'px';
    }
    if (attrName === 'current-mark-column' || attrName === 'line-number-at-mark' || attrName === 'mark-active') {
        this.updateMark();
    }
    if (attrName === 'tab-width') {
        this.style.tabSize = this.getAttribute('tab-width');
    }
};

DeuceBuffer.updateMark = () => {
    if (this.getAttribute('mark-active') !== 'true') {
        return;
    }
    let lineNumberAtPoint = this.intAttribute('line-number-at-point'),
        lineNumberAtMark = this.intAttribute('line-number-at-mark'),
        column = this.intAttribute('current-column'),
        markColumn = this.intAttribute('current-mark-column'),
        firstLineInClientBuffer = this.querySelector('line-d').intAttribute('number'),
        lastLineInClientBuffer = this.querySelector('line-d:last-child').intAttribute('number'),
        lineNumberAtStart = this.win.intAttribute('line-number-at-start'),
        visibleLineAtPoint = (lineNumberAtPoint - (firstLineInClientBuffer - lineNumberAtStart)) - lineNumberAtStart;

    if (lineNumberAtMark < lineNumberAtStart) {
        markColumn = 0;
    }
    lineNumberAtMark = Math.min(Math.max(lineNumberAtMark, lineNumberAtStart),
                                lineNumberAtStart + lastLineInClientBuffer);
    let visibleLineAtMark = (lineNumberAtMark - (firstLineInClientBuffer - lineNumberAtStart)) - lineNumberAtStart,
        startColumn, startLine, endColumn, endLine;

    if (visibleLineAtPoint < visibleLineAtMark) {
        startColumn = column;
        startLine = visibleLineAtPoint;
        endColumn = markColumn;
        endLine = visibleLineAtMark;
    } else if (visibleLineAtPoint > visibleLineAtMark) {
        startColumn = markColumn;
        startLine = visibleLineAtMark;
        endColumn = column;
        endLine = visibleLineAtPoint;
    } else {
        startColumn = Math.min(column, markColumn);
        startLine = visibleLineAtMark;
        endColumn = Math.max(column, markColumn);
        endLine = visibleLineAtPoint;
    }
    this.mark.update(startColumn, startLine, endColumn, endLine);
};

let DeuceWindow = Object.create(DeuceElement);

DeuceWindow.attachedCallback = () => {
    this.buffer = this.querySelector('buffer-d');
    this.resize = this.resize.bind(this);
    window.addEventListener('resize', this.resize);
    setTimeout(this.resize);
    DeuceElement.attachedCallback.call(this);
};

DeuceWindow.detachedCallback = () => {
    window.removeEventListener('resize', this.resize);
};

DeuceWindow.attributeChangedCallback = (attrName) => {
    if (!this.parentElement) {
        return;
    }
    if (attrName === 'line-number-at-start') {
        let firstLineInClientBuffer = this.buffer.querySelector('line-d').intAttribute('number'),
            lineNumberAtStart = this.intAttribute('line-number-at-start');
        if (![lineNumberAtStart, firstLineInClientBuffer].some(Number.isNaN)) {
            this.scrollTo(lineNumberAtStart - firstLineInClientBuffer);
        }
    }
};

DeuceWindow.scrollTo = (visibleLine) => {
    let negativeY = this.buffer.point.charHeight * -visibleLine;
    if (!Number.isNaN(negativeY)) {
        let transform = 'translate3d(' + 0 + 'px, ' + negativeY + 'px, 0)';
        this.buffer.display.style.transform = transform;
    }
};

let DeuceFrame = Object.create(DeuceElement);

DeuceFrame.attachedCallback = () => {
    this.rootWindow = this.querySelector('window-d:not([mini-p])');
    this.minibufferWindow = this.querySelector('window-d[mini-p]');
    this.resize = this.resize.bind(this);
    window.addEventListener('resize', this.resize);
    setTimeout(this.resize);
    DeuceElement.attachedCallback.call(this);
};

DeuceFrame.detachedCallback = () => {
    window.removeEventListener('resize', this.resize);
};

let tagPrototypes = {'buffer-d': DeuceBuffer, 'point-d': DeucePoint, 'mark-d': DeuceMark,
                     'window-d': DeuceWindow, 'frame-d': DeuceFrame};

document.addEventListener('DOMContentLoaded', () => {
    [].slice.call(document.querySelectorAll('template[data-tag]')).forEach((template) => {
        let tag = template.dataset.tag;
        document.registerElement(tag, {prototype: tagPrototypes[tag] || Object.create(DeuceElement)});
    });
});

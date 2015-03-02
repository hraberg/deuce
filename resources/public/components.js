/*eslint-env browser */
/*global ws */

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
    this.setAttribute('width', Math.round(rect.width / point.fontWidth));
    this.setAttribute('height', Math.round(rect.height / point.fontHeight));
};

let DeucePoint = Object.create(DeuceElement);

DeucePoint.attachedCallback = () => {
    let pointStyle = this.querySelector('::shadow span').getBoundingClientRect();
    this.fontWidth = parseFloat(pointStyle.width);
    this.fontHeight = parseFloat(pointStyle.height);
    DeuceElement.attachedCallback.call(this);
};

DeucePoint.moveTo = (column, visibleLine) => {
    if (![column, visibleLine, this.fontWidth, this.fontHeight].some(Number.isNaN)) {
        let x = (this.fontWidth * column),
            y = (this.fontHeight) * visibleLine,
            transform = 'translate3d(' + x + 'px, ' + y + 'px, 0)';
        this.style.transform = transform;
    }
};

let DeuceBuffer = Object.create(DeuceElement);

DeuceBuffer.attachedCallback = () => {
    this.point = this.querySelector('::shadow point-d');
    this.scrollBuffer = this.querySelector('::shadow .scroll-buffer');
    this.win = this.parentElement;
    DeuceElement.attachedCallback.call(this);
};

DeuceBuffer.attributeChangedCallback = (attrName) => {
    if (!this.parentElement) {
        return;
    }
    if (attrName === 'current-column' || attrName === 'line-number-at-point') {
        let column = parseInt(this.getAttribute('current-column'), 10),
            firstLineInClientBuffer = parseInt(this.querySelector('line-d').getAttribute('number'), 10),
            lineNumberAtPoint = parseInt(this.getAttribute('line-number-at-point'), 10),
            lineNumberAtStart = parseInt(this.win.getAttribute('line-number-at-start'), 10),
            visibleLine = (lineNumberAtPoint - (firstLineInClientBuffer - lineNumberAtStart)) - lineNumberAtStart;
        this.point.moveTo(column, visibleLine);
    }
    if (attrName === 'line-number-at-point-max') {
        let lineNumberAtPointMax = parseInt(this.getAttribute('line-number-at-point-max'), 10);
        this.scrollBuffer.style.height = (lineNumberAtPointMax * this.point.fontHeight) + 'px';
    }
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
        let firstLineInClientBuffer = parseInt(this.buffer.querySelector('line-d').getAttribute('number'), 10),
            lineNumberAtStart = parseInt(this.getAttribute('line-number-at-start'), 10);
        if (![lineNumberAtStart, firstLineInClientBuffer].some(Number.isNaN)) {
            this.scrollTo(lineNumberAtStart - firstLineInClientBuffer);
        }
    }
};

DeuceWindow.resize = () => {
    DeuceElement.resize.call(this);
    if (ws) {
        ws.send(JSON.stringify(['zw', this.getAttribute('sequence-number'),
                                parseInt(this.getAttribute('width'), 10), parseInt(this.getAttribute('height', 10))]));
    }
};

DeuceWindow.scrollTo = (visibleLine) => {
    let negativeY = this.buffer.point.fontHeight * -visibleLine;
    if (!Number.isNaN(negativeY)) {
        let transform = 'translate3d(' + 0 + 'px, ' + negativeY + 'px, 0)';
        this.buffer.style.transform = transform;
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

DeuceFrame.resize = () => {
    DeuceElement.resize.call(this);
    if (ws) {
        ws.send(JSON.stringify(['zf', this.getAttribute('name'),
                                parseInt(this.getAttribute('width'), 10), parseInt(this.getAttribute('height', 10))]));
    }
};

let tagPrototypes = {'buffer-d': DeuceBuffer, 'point-d': DeucePoint, 'window-d': DeuceWindow, 'frame-d': DeuceFrame};

document.addEventListener('DOMContentLoaded', () => {
    [].slice.call(document.querySelectorAll('template[data-tag]')).forEach((template) => {
        let tag = template.dataset.tag;
        document.registerElement(tag, {prototype: tagPrototypes[tag] || Object.create(DeuceElement)});
    });
});

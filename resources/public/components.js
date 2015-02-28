/*eslint-env browser */

'use strict';

let DeuceElement = Object.create(HTMLElement.prototype);

DeuceElement.createdCallback = () => {
    let template = document.querySelector('template[data-tag=' + this.tagName.toLowerCase() + ']').content,
        clone = document.importNode(template, true);
    this.createShadowRoot().appendChild(clone);
};

DeuceElement.attachedCallback = () => {
    let element = this;
    setTimeout(() => [].slice.call(element.attributes).forEach((attr) => {
        let value = attr.value,
           attributeChangedCallback = element.attributeChangedCallback;
        if (attributeChangedCallback) {
            attributeChangedCallback.call(element, attr.name, value, value);
        }
    }));
};

let DeucePoint = Object.create(DeuceElement);

DeucePoint.attachedCallback = () => {
    let pointStyle = window.getComputedStyle(this);
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
    DeuceElement.attachedCallback.call(this);
};

DeuceBuffer.attributeChangedCallback = (attrName) => {
    if (attrName === 'current-column' || attrName === 'line-number-at-point') {
        let column = parseInt(this.attributes['current-column'].value, 10),
            lineNumberAtPoint = parseInt(this.attributes['line-number-at-point'].value, 10),
            visibleLine = lineNumberAtPoint - 1;
        this.point.moveTo(column, visibleLine);
    }
};

let DeuceWindow = Object.create(DeuceElement);

DeuceWindow.attachedCallback = () => {
    this.buffer = this.querySelector('buffer-d');
    DeuceElement.attachedCallback.call(this);
};

DeuceWindow.attributeChangedCallback = (attrName) => {
    if (attrName === 'line-number-at-start') {
        let lineNumberAtStart = parseInt(this.attributes['line-number-at-start'].value, 10), win = this;
        win.scrollTo(lineNumberAtStart - 1);
    }
};

DeuceWindow.scrollTo = (visibleLine) => {
    let negativeY = this.buffer.point.fontHeight * -visibleLine;
    if (!Number.isNaN(negativeY)) {
        let transform = 'translate3d(' + 0 + 'px, ' + negativeY + 'px, 0)';
        this.buffer.style.transform = transform;
    }
};

let tagPrototypes = {'buffer-d': DeuceBuffer, 'point-d': DeucePoint, 'window-d': DeuceWindow};

document.addEventListener('DOMContentLoaded', () => {
    [].slice.call(document.querySelectorAll('template[data-tag]')).forEach((template) => {
        let tag = template.dataset.tag;
        document.registerElement(tag, {prototype: Object.create(tagPrototypes[tag] || DeuceElement)});
    });
});

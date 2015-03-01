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

DeuceBuffer.fromModel = (state, lineNumberAtStart) => {
    lineNumberAtStart = lineNumberAtStart || 1;
    let buffer = document.createElement('buffer-d');
    Object.keys(state).forEach((a) => {
        if (a === 'text') {
            let fragment = document.createDocumentFragment();
            state[a].forEach((l, idx) => {
                let line = document.createElement('line-d');
                line.setAttribute('number', idx + lineNumberAtStart);
                line.innerHTML = l;
                fragment.appendChild(line);
            });
            buffer.appendChild(fragment);
        } else if (a === 'minor-modes') {
            buffer.setAttribute(a, state[a].join(' '));
        } else {
            buffer.setAttribute(a, state[a]);
        }
    });
    return buffer;
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

DeuceWindow.scrollTo = (visibleLine) => {
    let negativeY = this.buffer.point.fontHeight * -visibleLine;
    if (!Number.isNaN(negativeY)) {
        let transform = 'translate3d(' + 0 + 'px, ' + negativeY + 'px, 0)';
        this.buffer.style.transform = transform;
    }
};

DeuceWindow.fromModel = (state) => {
    let win = document.createElement('window-d'),
    lineNumberAtStart = parseInt(state['line-number-at-start'], 10);
    Object.keys(state).forEach((a) => {
        if (a === 'buffer') {
            win.appendChild(DeuceBuffer.fromModel(state[a], lineNumberAtStart));
        } else if (a === 'mode-line') {
            let modeLine = document.createElement('mode-line-d');
            modeLine.innerHTML = state[a];
            win.appendChild(modeLine);
        } else {
            win.setAttribute(a, state[a]);
        }
    });
    return win;
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

DeuceFrame.fromModel = (state) => {
    let frame = document.createElement('frame-d');
    Object.keys(state).forEach((a) => {
        if (a === 'minor-modes') {
            frame.setAttribute(a, state[a].join(' '));
        } else if (a === 'menu-bar') {
            let menuBar = document.createElement('menu-bar-d'),
                fragment = document.createDocumentFragment();
            state[a].forEach((m) => {
                let menu = document.createElement('menu-d');
                menu.innerHTML = m;
                fragment.appendChild(menu);
            });
            menuBar.appendChild(fragment);
            frame.appendChild(menuBar);
        } else if (a === 'windows') {
            let fragment = document.createDocumentFragment();
            state[a].forEach((w) => {
                fragment.appendChild(DeuceWindow.fromModel(w));
            });
            frame.appendChild(fragment);
        } else {
            frame.setAttribute(a, state[a]);
        }
    });
    return frame;
};

let tagPrototypes = {'buffer-d': DeuceBuffer, 'point-d': DeucePoint, 'window-d': DeuceWindow, 'frame-d': DeuceFrame};

document.addEventListener('DOMContentLoaded', () => {
    [].slice.call(document.querySelectorAll('template[data-tag]')).forEach((template) => {
        let tag = template.dataset.tag;
        document.registerElement(tag, {prototype: tagPrototypes[tag] || Object.create(DeuceElement)});
    });
});

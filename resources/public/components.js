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
            state[a].forEach((l, idx) => {
                let line = document.createElement('line-d');
                line.setAttribute('number', idx + lineNumberAtStart);
                line.innerHTML = l;
                buffer.appendChild(line);
            });
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
            frame.setAttribute(a, (state[a] || []).join(' '));
        } else if (a === 'menu-bar') {
            let menuBar = document.createElement('menu-bar-d');
            state[a].forEach((m) => {
                let menu = document.createElement('menu-d');
                menu.innerHTML = m;
                menuBar.appendChild(menu);
            });
            frame.appendChild(menuBar);
        } else if (a === 'windows') {
            (state[a] || []).forEach((w) => {
                frame.appendChild(DeuceWindow.fromModel(w));
            });
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

    let state = {'frame':
                 {'menu-bar': ['File', 'Edit', 'Options', 'Tools', 'Buffers', 'Help'],
                  'minor-modes': ['blink-cursor-mode', 'menu-bar-mode'],
                  'windows': [
                      {'sequence-number': 0,
                       'line-number-at-start': 1,
                       'live-p': true,
                       'selected': true,
                       'buffer':
                       {'name': '*scratch*',
                        'line-number-at-point-max': 5,
                        'line-number-at-point': 5,
                        'current-column': 0,
                        'major-mode': 'lisp-interaction-mode',
                        'minor-modes': [],
                        'current': true,
                        'text': [';; This buffer is for notes you don\'t want to save, and for Lisp evaluation.', ';; If you want to create a file, visit that file with C-x C-f,', ';; then enter the text in that file\'s own buffer.', '', '']},
                       'mode-line': '-UUU:----F1  <strong>*scratch*</strong>      All L1     (Lisp Interaction) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'},
                      {'sequence-number': 1,
                       'line-number-at-start': 1,
                       'live-p': true,
                       'mini-p': true,
                       'buffer':
                       {'name': ' *Minibuf-0*',
                        'line-number-at-point-max': 1,
                        'line-number-at-point': 1,
                        'current-column': 0,
                        'major-mode': 'minibuffer-inactive-mode',
                        'minor-modes': [],
                        'text': ['Welcome to GNU Emacs']}}]}};

    if (state.frame) {
        document.body.appendChild(DeuceFrame.fromModel(state.frame));
    }
});

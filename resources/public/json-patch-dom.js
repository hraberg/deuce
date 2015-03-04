'use strict';

const jsdom = require('jsdom'),
      jsonpatch = require('fast-json-patch');

let document = jsdom.jsdom();

function toDOM(e) {
    if (typeof e === 'string') {
        return document.createTextNode(e);
    }
    let tag = e[0],
        attributes = e[1] || {},
        children = e.slice(2) || [];
    return children.map(toDOM).reduce((dom, c) => {
        dom.appendChild(c);
        return dom;
    }, Object.keys(attributes).reduce((dom, a) => {
        dom.setAttribute(a, attributes[a]);
        return dom;
    }, document.createElement(tag)));
}

function patchAttribute(attributes, name, value) {
    let attr = attributes[name] || document.createAttribute(name);
    attr.value = value;
    attributes.setNamedItem(attr);
}

function resolvePointer(element, path) {
    let segs = path.slice(1).split('/').map((x) => parseInt(x, 10) || x);
    return segs.reduce((acc, seg) => {
        let parent = acc[0],
            child;
        if (seg === 1) {
            child = parent.attributes;
        } else if (parent.childNodes) {
            child = seg === '-' ? undefined : parent.childNodes[seg - 2];
        } else {
            child = seg;
        }
        return [child, parent];
    }, [element]);
}

// https://tools.ietf.org/html/rfc6902
function doPatch(element, p) {
    let resolved = resolvePointer(element, p.path),
        child = resolved[0],
        parent = resolved[1];

    if (p.op === 'add') {
        if (typeof child === 'string') {
            patchAttribute(parent, child, p.value);
        } else if (child) {
            parent.insertBefore(toDOM(p.value), child);
        } else {
            parent.appendChild(toDOM(p.value));
        }
    }
    if (p.op === 'replace') {
        if (typeof child === 'string') {
            patchAttribute(parent, child, p.value);
        } else {
            if (child.nodeType === 3) {
                child.data = p.value;
            } else {
                parent.replaceChild(toDOM(p.value), child);
            }
        }
    }
    if (p.op === 'remove') {
        if (typeof child === 'string') {
            parent.removeNamedItem(child);
        } else {
            parent.removeChild(child);
        }
    }
    if (p.op === 'move' || p.op === 'copy') {
        let resolved = resolvePointer(element, p.from),
            fromChild = resolved[0],
            fromParent = resolved[1];

        if (p.op === 'move') {
            if (typeof fromChild === 'string') {
                fromParent.removeNamedItem(fromChild);
                patchAttribute(parent, child, fromChild.value);
            } else if (child) {
                fromParent.removeChild(fromChild);
                parent.insertBefore(fromChild, child);
            } else {
                fromParent.removeChild(fromChild);
                parent.appendChild(fromChild);
            }
        }
        if (p.op === 'copy') {
            if (typeof fromChild === 'string') {
                patchAttribute(parent, child, fromChild.value);
            } else if (child) {
                parent.insertBefore(fromChild.cloneNode(true), child);
            } else {
                parent.appendChild(fromChild.cloneNode(true));
            }
        }
    }
}

// Based on the example from the Ember Glimer presentation:
// http://f.cl.ly/items/0t031v2Z3y001V1N0F3N/Virtual%20DOM.pdf
let jsonml1 = ['div', {class: 'item'},
               ['p', {}, 'It\'s enabled'],
               ['p', {}, 'Tom Dale'],
               ['p', {class: 'popover'},
                'A True Scotsman']];

let jsonml2 = ['div', {class: 'item'},
               ['p', {}, 'It\'s enabled'],
               ['p', {}, 'Thomas Dale'],
               ['p', {class: 'popover'},
                'A True Scotsman']];

let jsonml3 = ['div', {class: 'done'},
               ['p', {}, 'It\'s enabled'],
               ['p', {}, 'Thomas Dale'],
               ['p', {class: 'popover', style: 'fontweight: bold;'},
                'A True Scotsman']];

let jsonml4 = ['div', {},
               ['p', {}, 'It\'s enabled'],
               ['p', {}, 'Thomas Dale'],
               ['p', {class: 'popover', style: 'fontweight: bold;'},
                'A True Scotsman']];

let jsonml5 = ['div', {},
               ['p', {}, 'It\'s enabled'],
               ['p', {class: 'popover', style: 'fontweight: bold;'},
                'A True Scotsman']];

let jsonml6 = ['div', {},
               ['p', {class: 'popover', style: 'fontweight: bold;'},
                'A True Scotsman'],
               ['p', {}, 'It\'s enabled']];

console.log(document.body.outerHTML);

function applyPatches(element, from, to) {
    console.time('jsonpatch');
    let patch = jsonpatch.compare(from, to);
    console.timeEnd('jsonpatch');
    console.time('json');
    console.log('patch size:', JSON.stringify(patch).length);
    console.timeEnd('json');
    console.log('json size:', JSON.stringify(to).length);
    console.time('dom');
    patch.forEach(doPatch.bind(null, element));
    console.timeEnd('dom');
    console.log(element.outerHTML);
}

applyPatches(document.body, ['body', {}], ['body', {}, jsonml1]);
applyPatches(document.body, ['body', {}, jsonml1], ['body', {}, jsonml2]);
applyPatches(document.body, ['body', {}, jsonml2], ['body', {}, jsonml3]);
applyPatches(document.body, ['body', {}, jsonml3], ['body', {}, jsonml4]);
applyPatches(document.body, ['body', {}, jsonml4], ['body', {}, jsonml5]);
applyPatches(document.body, ['body', {}, jsonml5], ['body', {}, jsonml6]);

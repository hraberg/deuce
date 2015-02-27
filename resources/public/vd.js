/*eslint-env browser */

'use strict';

let existingElements = {},
    accessedElements = {};

function text(s) {
    return {text: s, element: document.createTextNode(s), _type: 'text'};
}

function e(tag, key) {
    let attributes = {},
        children,
        virtualElement = existingElements[key];

    if (typeof arguments[2] === 'object' && !arguments[2]._type) {
        attributes = arguments[2];
        children = [].slice.call(arguments).slice(3) || [];
    } else {
        children = [].slice.call(arguments).slice(2) || [];
    }

    if (virtualElement && virtualElement.tag === tag) {
        let element = virtualElement.element;

        Object.keys(attributes).forEach((k) => {
            if (attributes[k] === undefined) {
                element.removeAttribute(k);
            } else {
                element.setAttribute(k, attributes[k]);
            }
            delete virtualElement.attributes[k];
        });

        Object.keys(virtualElement.attributes).forEach((k) => {
            element.removeAttribute(k);
        });

        virtualElement.attributes = attributes;

        let oldIndex = 0;
        children = children.map((newChild) => {
            let isString = typeof newChild === 'string',
                oldChild = virtualElement.children[oldIndex],
                newVirtualChild = () => isString ? text(newChild) : newChild;

            if (oldChild && (newChild.key && newChild.key !== oldChild.key ||
                             isString && newChild !== oldChild.text)) {
                newChild = newVirtualChild();
                element.insertBefore(newChild.element, oldChild.element);

            } else if (!oldChild) {
                newChild = newVirtualChild();
                element.appendChild(newChild.element);

            } else {
                if (isString) {
                    newChild = newChild === oldChild.text ? oldChild : text(newChild);
                }
                oldIndex += 1;
            }

            return newChild;
        });

        for (let toRemove = element.childNodes.length - children.length; toRemove > 0; toRemove -= 1) {
            element.lastChild.remove();
        }

        virtualElement.children = children;

    } else {
        let element = document.createElement(tag);
        element.key = key;

        Object.keys(attributes).forEach((k) => {
            element.setAttribute(k, attributes[k]);
        });

        children = children.map((c) => {
            return typeof c === 'string' ? text(c) : c;
        });

        children.forEach((c) => element.appendChild(c.element));

        virtualElement = {tag: tag, key: key, attributes: attributes, children: children,
                          element: element, _type: 'node'};
    }

    accessedElements[key] = virtualElement;
    return virtualElement;
}

function redraw(render) {
    console.time('render');
    try {
        return render();
    } finally {
        existingElements = accessedElements;
        accessedElements = {};
        console.timeEnd('render');
    }
}

document.addEventListener('DOMContentLoaded', () => {
    let state = {count: 1},
        render = (s) => {
            if (s.count % 2 === 0) {
                return e('span', 1, {'foo': 'bar'},
                         e('b', 2, 'Another Tag'), 'Hello World ' + s.count);
            }
            return e('span', 1, {'data-count': s.count},
                     'Hello World ' + s.count, e('b', 2, 'Another Tag'));
        };

    document.body.appendChild(redraw(render.bind(null, state)).element);

    let pendingRefresh = false;
    setInterval(() => {
        state.count += 1;
        if (!pendingRefresh) {
            pendingRefresh = true;
            requestAnimationFrame(() => {
                pendingRefresh = false;
                redraw(render.bind(null, state));
            });
        }
    }, 1000);
});

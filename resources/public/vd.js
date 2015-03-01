/*eslint-env browser */

'use strict';

let DeuceVDom = (() => {
    let existingElements = {},
        accessedElements = {};

    let text = (s, key) => {
        let virtualElement = existingElements[key];
        if (!virtualElement) {
            virtualElement = {text: s, key: key, element: document.createTextNode(s), _type: 'text'};
        } else if (virtualElement.text !== s) {
            virtualElement.text = s;
            virtualElement.element.data = s;
        }

        if (key) {
            accessedElements[key] = virtualElement;
        }
        return virtualElement;
    };

    let e = (tag) => {
        let attributes = {},
            children;

        if (typeof arguments[1] === 'object' && !arguments[1]._type) {
            attributes = arguments[1];
            children = [].slice.call(arguments).slice(2) || [];
        } else {
            children = [].slice.call(arguments).slice(1) || [];
        }
        if (Array.isArray(children[children.length - 1])) {
            children = children.slice(0, children.length - 1).concat(children[children.length - 1]);
        }
        let key = attributes.key,
            virtualElement = existingElements[key];

        if (virtualElement && virtualElement.tag === tag) {
            let element = virtualElement.element;

            Object.keys(attributes).forEach((k) => {
                if (attributes[k] === undefined) {
                    element.removeAttribute(k);
                } else {
                    if (k === 'style') {
                        Object.keys(attributes.style).forEach((s) => {
                            if (attributes.style[s] !== undefined) {
                                element.style[s] = attributes.style[s];
                            } else {
                                element.style[s] = null;
                            }
                            if (virtualElement.attributes.style) {
                                delete virtualElement.attributes.style[s];
                            }
                        });
                        Object.keys(virtualElement.attributes.style || {}).forEach((s) => {
                            element.style.removeProperty(s);
                        });
                    } else if (k === 'data') {
                        Object.keys(attributes.data).forEach((d) => {
                            if (attributes.data[d] !== undefined) {
                                element.dataset[d] = attributes.data[d];
                            } else {
                                delete element.dataset[d];
                            }
                            if (virtualElement.attributes.data) {
                                delete virtualElement.attributes.data[d];
                            }
                        });
                        Object.keys(virtualElement.attributes.data || {}).forEach((d) => {
                            delete element.dataset[d];
                        });
                    } else if (k === 'innerHTML') {
                        element.innerHTML = attributes.innerHTML;
                    } else {
                        let attr = attributes[k];
                        element.setAttribute(k, Array.isArray(attr) ? attr.join(' ') : attr);
                    }
                }
                delete virtualElement.attributes[k];
            });

            if (!attributes.data) {
                Object.keys(element.dataset).forEach((d) => {
                    delete element.dataset[d];
                });
            }

            Object.keys(virtualElement.attributes).forEach((k) => {
                element.removeAttribute(k);
            });

            virtualElement.attributes = attributes;

            if (!attributes.innerHTML) {
                let oldIndex = 0;
                children = children.map((newChild, idx) => {
                    let isString = typeof newChild === 'string',
                        oldChild = virtualElement.children[oldIndex],
                        newVirtualChild = () => isString ? text(newChild, key + '-text-' + idx) : newChild;

                    if (oldChild && (newChild.key && newChild.key !== oldChild.key ||
                                     isString && newChild !== oldChild.text)) {
                        newChild = newVirtualChild();
                        element.insertBefore(newChild.element, oldChild.element);

                    } else if (!oldChild) {
                        newChild = newVirtualChild();
                        element.appendChild(newChild.element);

                    } else {
                        if (isString) {
                            newChild = newChild === oldChild.text ? oldChild : text(newChild, key + '-text-' + idx);
                        }
                        oldIndex += 1;
                    }

                    return newChild;
                });

                for (let toRemove = element.childNodes.length - children.length; toRemove > 0; toRemove -= 1) {
                    element.lastChild.remove();
                }

                virtualElement.children = children;
            }

        } else {
            let element = document.createElement(tag);
                element.key = key;

            Object.keys(attributes).forEach((k) => {
                if (k === 'style') {
                    Object.keys(attributes.style).forEach((s) => {
                        element.style[s] = attributes.style[s];
                    });
                } else if (k === 'data') {
                    Object.keys(attributes.data).forEach((d) => {
                        element.dataset[d] = attributes.data[d];
                    });
                } else if (k === 'innerHTML') {
                    element.innerHTML = attributes.innerHTML;
                } else {
                    let attr = attributes[k];
                    element.setAttribute(k, Array.isArray(attr) ? attr.join(' ') : attr);
                }
            });

            if (!attributes.innerHTML) {
                children = children.map((c, idx) => {
                    return typeof c === 'string' ? text(c, key + '-text-' + idx) : c;
                });
                children.forEach((c) => element.appendChild(c.element));
            }

            virtualElement = {tag: tag, key: key, attributes: attributes, children: children,
                              element: element, _type: 'node'};
        }

        accessedElements[key] = virtualElement;
        return virtualElement;
    };

    let redraw = (render) => {
        try {
            return render();
        } finally {
            existingElements = accessedElements;
            accessedElements = {};
        }
    };

    return {redraw: redraw, e: e};
})();

if (typeof module !== 'undefined' && module !== null && module.exports) {
    module.exports = DeuceVDom;
}

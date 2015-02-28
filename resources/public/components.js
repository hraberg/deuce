/*eslint-env browser */

'use strict';

document.addEventListener('DOMContentLoaded', () => {
    [].slice.call(document.querySelectorAll('template[data-tag]')).forEach((template) => {
        let tag = template.dataset.tag,
            proto = Object.create(HTMLElement.prototype);
        proto.createdCallback = () => {
            let clone = document.importNode(template.content, true);
            this.createShadowRoot().appendChild(clone);
        };
        document.registerElement(tag, {prototype: proto});
    });
});

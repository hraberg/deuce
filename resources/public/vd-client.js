/*eslint-env browser */

'use strict';

const diff = require('virtual-dom/diff'),
      patch = require('virtual-dom/patch'),
      VNode = require('virtual-dom/vnode/vnode'),
      VText = require('virtual-dom/vnode/vtext'),
      htmlToVdom = require('html-to-vdom');

const convertHTML = htmlToVdom({VNode: VNode, VText: VText});

function applySimpleCharDiffs(ds, s) {
    let acc = '';
    for (let i = 0, idx = 0; i < ds.length; i += 1) {
        let d = ds[i];
        if (typeof d === 'string') {
            acc += d;
        } else if (d > 0) {
            acc += s.slice(idx, idx + d);
            idx += d;
        } else if (d < 0) {
            idx -= d;
        }
    }
    return acc;
}

function applySimpleLineDiffs(rootElement, ds) {
    let template = document.createElement('template');
    for (let i = 0, idx = 0; i < ds.length; i += 1) {
        let d = ds[i], node = rootElement.childNodes[idx];
        if (Array.isArray(d)) {
            let line = ((node && node.outerHTML) || '');
            template.innerHTML = applySimpleCharDiffs(d, line);
            if (node) {
                rootElement.replaceChild(template.content, node);
            } else {
                rootElement.appendChild(template.content);
            }
        } else if (typeof d === 'string') {
            template.innerHTML = d;
            if (node) {
                rootElement.insertBefore(template.content, node);
            } else {
                rootElement.appendChild(template.content);
            }
            idx += d.split(/\n/gm).length;
        } else if (d > 0) {
            idx += d;
        } else if (d < 0) {
            while (d < 0) {
                node.remove();
                d += 1;
            }
        }
    }
    return rootElement;
}

let useVirtualDom = false,
    html,
    tree,
    revision,
    rootNode,
    pendingRefresh,
    pendingPatches = [],
    clientCompileTime;

function onrefresh(newRevision, newHtml, newClientCompileTime) {
    if (!clientCompileTime) {
        clientCompileTime = newClientCompileTime;
    }
    if (clientCompileTime !== newClientCompileTime) {
        console.log('new client version, reloading app');
        window.location.reload();
    }
    html = newHtml;
    revision = newRevision;
}

function patchCommon(oldRevision) {
    if (revision === undefined) {
        console.log('got patch before full refresh, ignoring.');
        return;
    }
    if (oldRevision !== revision) {
        console.log('out of sync with server, requesting refresh:', oldRevision, revision);
        revision = undefined;
        ws.send(JSON.stringify(['r']));
        return;
    }
}

function onpatchChars(oldRevision, diffs) {
    patchCommon(oldRevision);
    console.time('patch chars');
    html = applySimpleCharDiffs(diffs, html || rootNode.innerHTML);
    revision = oldRevision + 1;
    console.timeEnd('patch chars');
}

function onpatchLines(oldRevision, diffs) {
    patchCommon(oldRevision);
    pendingPatches.push(diffs);
    html = undefined;
    revision = oldRevision + 1;
}

function render(serverTime) {
    if (useVirtualDom) {
        console.time('htmlToVdom');
        let newTree = convertHTML(html);
        console.timeEnd('htmlToVdom');
        pendingPatches.push(diff(tree, newTree));
        tree = newTree;
    }
    if (!pendingRefresh) {
        pendingRefresh = true;
        requestAnimationFrame(() => {
            console.time('redraw');
            if (useVirtualDom) {
                while (pendingPatches.length > 0) {
                    rootNode = patch(rootNode, pendingPatches.shift());
                }
            } else {
                if (html) {
                    rootNode.innerHTML = html;
                }
                if (pendingPatches.length > 0) {
                    console.time('patch lines');
                    while (pendingPatches.length > 0) {
                        rootNode = applySimpleLineDiffs(rootNode, pendingPatches.shift());
                    }
                    console.timeEnd('patch lines');
                }
            }
            pendingRefresh = false;
            console.timeEnd('redraw');
            console.log('latency:', Date.now() - serverTime, 'ms');
        });
    }
}

function onmessage(data) {
    console.log('client received:', data.data.length, data.data);
    console.time('parse');
    let message = JSON.parse(data.data);
    console.timeEnd('parse');
    ({r: onrefresh, c: onpatchChars, l: onpatchLines})[message[0]].apply(null, message.slice(1));
    render(message[message.length - 1]);
}

let url = 'ws://127.0.0.1:8080',
    ws,
    initialReconnectInterval = 1000,
    maxReconnectInterval = initialReconnectInterval * 5,
    reconnectInterval = initialReconnectInterval,
    reconnectBackoffRatio = 1.2;

function connect() {
    if (ws) {
        return;
    }
    console.log('connecting to', url);

    ws = new WebSocket(url + '?lines=' + !useVirtualDom);
    ws.onmessage = onmessage;
    ws.onopen = (e) => {
        console.log('connection opened:', e);
        reconnectInterval = initialReconnectInterval;
    };
    ws.onerror = (e) => {
        console.log('connection error:', e);
        ws.close();
    };
    ws.onclose = (e) => {
        console.log('connection closed:', e);
        console.log('retrying in:', reconnectInterval, 'ms.');
        ws = undefined;
        window.setTimeout(connect, reconnectInterval);
        reconnectInterval *= reconnectBackoffRatio;
        reconnectInterval = Math.min(maxReconnectInterval, reconnectInterval);
    };
}

window.addEventListener('error', (e) => {
    if (ws) {
        console.log('error, reloading app:', e);
        ws.close();
        ws = {};
        setTimeout(() => window.location.reload(), maxReconnectInterval);
    }
});

document.addEventListener('DOMContentLoaded', () => {
    if (useVirtualDom) {
        rootNode = document.createElement('div');
        document.body.appendChild(rootNode);
        tree = convertHTML(rootNode.outerHTML);
    } else {
        rootNode = document.body;
    }
    connect();
});

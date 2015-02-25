/*eslint-env browser */

'use strict';

const diff = require('virtual-dom/diff'),
      patch = require('virtual-dom/patch'),
      VNode = require('virtual-dom/vnode/vnode'),
      VText = require('virtual-dom/vnode/vtext'),
      htmlToVdom = require('html-to-vdom'),
      DiffDom = require('diff-dom'),
      jsonpatch = require('fast-json-patch');

const convertHTML = htmlToVdom({VNode: VNode, VText: VText});

function clientSideRender(s) {
    let count = s.count;
    return '<div style=\"text-align:center;line-height:' + (100 + count) +
        'px;border:1px solid red;width:' + (100 + count) + 'px;height:' +
        (100 + count) + 'px;\">' + count + '</div>';
}

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
            idx += 1;
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

let useVirtualDom = true,
    useDiffDom = false,
    useJSON = false && useVirtualDom,
    html,
    tree,
    state = {},
    revision,
    rootNode,
    pendingRefresh,
    pendingDomPatches = [],
    pendingVDomPatches = [],
    pendingLinePatches = [],
    clientCompileTime,
    dd = new DiffDom();

function onrefresh(newRevision, newHtml, newState, newClientCompileTime) {
    if (!clientCompileTime) {
        clientCompileTime = newClientCompileTime;
    }
    if (clientCompileTime !== newClientCompileTime) {
        console.log('new client version, reloading app');
        window.location.reload();
    }
    state = newState;
    html = newHtml;
    revision = newRevision;
}

function patchCommon(oldRevision) {
    if (revision === undefined) {
        console.log('got patch before full refresh, ignoring.');
        return false;
    }
    if (oldRevision !== revision) {
        console.log('out of sync with server, requesting refresh:', oldRevision, revision);
        revision = undefined;
        ws.send(JSON.stringify(['r']));
        return false;
    }
    return true;
}

function onpatchChars(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        console.time('patch chars');
        html = applySimpleCharDiffs(diffs, html || rootNode.innerHTML);
        revision = oldRevision + 1;
        console.timeEnd('patch chars');
    }
}

function onpatchLines(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        pendingLinePatches.push(diffs);
        html = undefined;
        revision = oldRevision + 1;
    }
}

function onpatchDom(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        pendingDomPatches.push(diffs);
        html = undefined;
        revision = oldRevision + 1;
    }
}

function onpatchJSON(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        if (!jsonpatch.apply(state, diffs)) {
            throw new Error('failed to apply patch' + JSON.stringify(diffs));
        }
        html = clientSideRender(state);
        revision = oldRevision + 1;
    }
}

function render(serverTime) {
    if (useVirtualDom) {
        let newTree = convertHTML(html);
        pendingVDomPatches.push(diff(tree, newTree));
        tree = newTree;
    }
    if (!pendingRefresh) {
        pendingRefresh = true;
        requestAnimationFrame(() => {
            console.time('redraw');
            if (useVirtualDom) {
                console.time('patch vdom');
                while (pendingVDomPatches.length > 0) {
                    rootNode = patch(rootNode, pendingVDomPatches.shift());
                }
                console.timeEnd('patch vdom');
            } else {
                if (html) {
                    rootNode.innerHTML = html;
                }
                if (pendingLinePatches.length > 0) {
                    console.time('patch lines');
                    while (pendingLinePatches.length > 0) {
                        rootNode = applySimpleLineDiffs(rootNode, pendingLinePatches.shift());
                    }
                    console.timeEnd('patch lines');
                }
                if (pendingDomPatches.length > 0) {
                    console.time('patch dom');
                    while (pendingDomPatches.length > 0) {
                        dd.apply(rootNode, pendingDomPatches.shift());
                    }
                    console.timeEnd('patch dom');
                }
            }
            pendingRefresh = false;
            console.timeEnd('redraw');
            console.log('latency:', Date.now() - serverTime, 'ms');
        });
    }
}

let handlers = {r: onrefresh,
                c: onpatchChars, l: onpatchLines,
                d: onpatchDom, j: onpatchJSON};

function onmessage(data) {
    console.log('client received:', data.data.length, data.data);
    console.time('parse');
    let message = JSON.parse(data.data);
    console.timeEnd('parse');
    handlers[message[0]].apply(null, message.slice(1));
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

    ws = new WebSocket(url + '?lines=' + !(useVirtualDom || useDiffDom) + '&dom=' + useDiffDom + '&json=' + useJSON);
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

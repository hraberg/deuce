/*eslint-env browser */

// browserify vd-client.js > vd-client-bundle.js

'use strict';

const diff = require('virtual-dom/diff'),
      patch = require('virtual-dom/patch'),
      VNode = require('virtual-dom/vnode/vnode'),
      VText = require('virtual-dom/vnode/vtext'),
      htmlToVdom = require('html-to-vdom');

const convertHTML = htmlToVdom({VNode: VNode, VText: VText});

function applySimpleDiffs(ds, s) {
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

let useVirtualDom = true,
    html,
    tree,
    revision,
    rootNode,
    pendingRefresh,
    pendingPatches = [],
    clientCompileTime;

function onrefresh (message) {
    if (!clientCompileTime) {
        clientCompileTime = message.clientCompileTime;
    }
    if (clientCompileTime !== message.clientCompileTime) {
        console.log('new client version, reloading app');
        setTimeout(() => window.location.reload(), 1000);
    }
    html = message.html;
    revision = message.revision;
}

function onpatch (message) {
    if (revision === undefined) {
        console.log('got patch before full refresh, ignoring.');
        return;
    }
    if (message.from !== revision) {
        console.log('out of sync with server, requesting refresh:', message.from, revision);
        revision = undefined;
        ws.send(JSON.stringify({'type': 'refresh'}));
        return;
    }
    console.time('patch');
    html = applySimpleDiffs(message.diff, html);
    revision = message.to;
    console.timeEnd('patch');
}

function render () {
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
                rootNode.innerHTML = html;
            }
            pendingRefresh = false;
            console.timeEnd('redraw');
        });
    }
}

function onmessage (data) {
    console.log('client received:', data.data.length, data.data);
    console.time('parse');
    let message = JSON.parse(data.data);
    console.timeEnd('parse');
    ({refresh: onrefresh, patch: onpatch})[message.type](message);
    render();
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

    ws = new WebSocket(url);
    ws.onmessage = onmessage;
    ws.onopen = (e) => {
        console.log('connection opened:', e);
        reconnectInterval = initialReconnectInterval;
    };
    ws.onerror = (e) => console.log('connection error:', e);
    ws.onclose = (e) => {
        console.log('connection closed:', e);
        console.log('retrying in:', reconnectInterval, 'ms.');
        ws = undefined;
        window.setTimeout(connect, reconnectInterval);
        reconnectInterval *= reconnectBackoffRatio;
        reconnectInterval = Math.min(maxReconnectInterval, reconnectInterval);
    };
}

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

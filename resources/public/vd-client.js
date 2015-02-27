/*eslint-env browser */

'use strict';

const h = require('virtual-dom/h'),
      diff = require('virtual-dom/diff'),
      patch = require('virtual-dom/patch'),
      VNode = require('virtual-dom/vnode/vnode'),
      VText = require('virtual-dom/vnode/vtext'),
      htmlToVdom = require('html-to-vdom'),
      m = require('mithril'),
      DiffDom = require('diff-dom'),
      jsonpatch = require('fast-json-patch');

const convertHTML = htmlToVdom({VNode: VNode, VText: VText});

function clientSideRender(s) {
    let count = s.count;
    return '<div style=\"text-align:center;line-height:' + (100 + count) +
        'px;border:1px solid red;width:' + (100 + count) + 'px;height:' +
        (100 + count) + 'px;\">' + count + '</div>';
}

function clientSideVirtualDomRender(s) {
    let count = s.count;
    return h('div', {
        style: {
            textAlign: 'center',
            lineHeight: (100 + count) + 'px',
            border: '1px solid red',
            width: (100 + count) + 'px',
            height: (100 + count) + 'px'
        }
    }, [String(count)]);
}

function clientSideMithrilRender(s) {
    let count = s.count;
    return m('div[style=text-align:center;line-height:' + (100 + count) +
             'px;border:1px solid red;width:' + (100 + count) + 'px;height:' + (100 + count) + 'px]',
             String(count));
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

let useVirtualDom = false,
    useDiffDom = false,
    useMithril = false,
    useJSON = (false && useVirtualDom) || (false && useMithril),
    useStateDiff = (false && useVirtualDom) || (false && useMithril),
    useVirtualDomRender = false && useVirtualDom && (useJSON || useStateDiff),
    html,
    tree,
    state = {},
    serializedState = JSON.stringify(state),
    revision,
    rootNode,
    pendingRefresh,
    pendingDomPatches = [],
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
    serializedState = JSON.stringify(state);
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

function onpatchDom(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        pendingDomPatches.push(diffs);
        html = undefined;
        revision = oldRevision + 1;
    }
}

function onpatchJSON(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        console.time('patch json');
        try {
            if (!jsonpatch.apply(state, diffs)) {
                throw new Error('failed to apply patch' + JSON.stringify(diffs));
            }
        } finally {
            console.timeEnd('patch json');
        }
        html = undefined;
        revision = oldRevision + 1;
    }
}

function onpatchState(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        console.time('patch state');
        serializedState = applySimpleCharDiffs(diffs, serializedState);
        state = JSON.parse(serializedState);
        console.timeEnd('patch state');
        html = undefined;
        revision = oldRevision + 1;
    }
}

function render(serverTime) {
    if (!pendingRefresh) {
        pendingRefresh = true;
        requestAnimationFrame(() => {
            console.time('redraw');
            if (useVirtualDom) {
                let newTree;
                console.time('patch vdom');
                if (useVirtualDomRender) {
                    newTree = clientSideVirtualDomRender(state);
                } else {
                    if (useJSON || useStateDiff) {
                        html = clientSideRender(state);
                    }
                    newTree = convertHTML(html);
                }
                rootNode = patch(rootNode, diff(tree, newTree));
                tree = newTree;
                console.timeEnd('patch vdom');
            } else if (useMithril) {
                console.time('patch mithril');
                m.render(rootNode, clientSideMithrilRender(state));
                console.timeEnd('patch mithril');
            } else {
                if (html) {
                    rootNode.innerHTML = html;
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
            console.timeEnd('client time');
            console.log('latency:', Date.now() - serverTime, 'ms');
        });
    }
}

let handlers = {r: onrefresh, c: onpatchChars,
                d: onpatchDom, j: onpatchJSON, s: onpatchState};

function onmessage(data) {
    console.log('client received:', data.data.length, data.data);
    console.time('client time');
    console.time('parse');
    let message = JSON.parse(data.data),
        serverTime = message[message.length - 1];
    console.timeEnd('parse');
    console.log('server time:', Date.now() - serverTime, 'ms');
    handlers[message[0]].apply(null, message.slice(1));
    render(serverTime);
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

    ws = new WebSocket(url + '?dom=' + useDiffDom + '&json=' + useJSON + '&state=' + useStateDiff);
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

/*eslint-env browser */
/*globals virtualDom m DeuceVDom */

'use strict';

let DEBUG = false,
    h = virtualDom.h;

let debug = () => {
    if (DEBUG) {
        console.debug.apply(console, [].slice.call(arguments));
    }
};

let renderers = new Map([[m, 'mithril'],
                         [DeuceVDom.e, 'deuce-vdom'],
                         [virtualDom.h, 'virtual-dom']]);

let usedRenderer = () => {
    return renderers.get(h);
};

let attrs = (properties) => {
    if (usedRenderer() === 'virtual-dom') {
        return properties;
    }
    let attributes = properties.attributes;
    if (attributes) {
        Object.keys(attributes).forEach((k) => {
            properties[k] = attributes[k];
        });
    }
    delete properties.attributes;
    return properties;
};

let attributeFromModel = (value) => {
    return Array.isArray(value) ? value.join(' ') : String(value);
};

let lineFromModel = (bufferName, lineNumberAtStart, line, idx) => {
    return h('line-d', attrs({key: 'line-' + bufferName + '-' + idx,
                              attributes: {number: (idx + lineNumberAtStart)}, innerHTML: line}));
};

let bufferFromModel = (state, lineNumberAtStart) => {
    let properties = {key: 'buffer-' + state.name, attributes: {}},
        children = [];
    lineNumberAtStart = lineNumberAtStart || 1;
    Object.keys(state).forEach((a) => {
        if (a === 'text') {
            state[a].map((line, idx) => lineFromModel(state.name, lineNumberAtStart, line, idx))
                .forEach((line) => children.push(line));
        } else {
            properties.attributes[a] = attributeFromModel(state[a]);
        }
    });
    return h('buffer-d', attrs(properties), children);
};

let windowFromModel = (state) => {
    let properties = {key: 'window-' + state['sequence-number'], attributes: {}},
        children = [],
        lineNumberAtStart = parseInt(state['line-number-at-start'], 10);
    Object.keys(state).forEach((a) => {
        if (a === 'buffer') {
            children.push(bufferFromModel(state[a], lineNumberAtStart));
        } else if (a === 'mode-line') {
            children.push(h('mode-line-d', attrs({key: 'mode-line-' + properties.key, innerHTML: state[a]})));
        } else {
            properties.attributes[a] = String(state[a]);
        }
    });
    return h('window-d', attrs(properties), children);
};

let frameFromModel = (state) => {
    let properties = {key: 'frame-0', attributes: {}},
        children = [];
    Object.keys(state).forEach((a) => {
        if (a === 'menu-bar') {
            children.push(h('menu-bar-d', attrs({key: 'menu-bar-' - properties.key}),
                            state[a].map((menu) => h('menu-d', {key: 'menu-' + menu}, menu))));
        } else if (a === 'windows') {
            state[a].map(windowFromModel).forEach((w) => children.push(w));
        } else {
            properties.attributes[a] = attributeFromModel(state[a]);
        }
    });
    return h('frame-d', attrs(properties), children);
};

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

let state,
    serializedState,
    revision,
    rootNode,
    vdomTree,
    pendingRefresh,
    clientCompileTime;

function onrefresh(newRevision, newState, newClientCompileTime) {
    if (!clientCompileTime) {
        clientCompileTime = newClientCompileTime;
    }
    if (clientCompileTime !== newClientCompileTime) {
        debug('new client version, reloading app');
        window.location.reload();
    }
    state = newState;
    serializedState = JSON.stringify(state);

    requestAnimationFrame(() => {
        if (usedRenderer() === 'mithril') {
            rootNode = document.body;
        } else if (usedRenderer() === 'deuce-vdom') {
            document.body.innerHTML = '';
            document.body.appendChild(DeuceVDom.redraw(() => frameFromModel(state.frame)).element);
        } else {
            vdomTree = frameFromModel(state.frame);
            rootNode = virtualDom.create(vdomTree);
            document.body.innerHTML = '';
            document.body.appendChild(rootNode);
        }
    });

    revision = newRevision;
}

function patchCommon(oldRevision) {
    if (revision === undefined) {
        console.error('got patch before full refresh, ignoring.');
        return false;
    }
    if (oldRevision !== revision) {
        console.error('out of sync with server, requesting refresh:', oldRevision, revision);
        revision = undefined;
        ws.send(JSON.stringify(['r']));
        return false;
    }
    return true;
}

function onpatch(oldRevision, diffs) {
    if (patchCommon(oldRevision)) {
        serializedState = applySimpleCharDiffs(diffs, serializedState);
        state = JSON.parse(serializedState);
        revision = oldRevision + 1;
    }
}

function render(serverTime) {
    if (!pendingRefresh) {
        pendingRefresh = true;
        console.time('frame waiting time');
        requestAnimationFrame(() => {
            console.timeEnd('frame waiting time');
            console.time('redraw');
            pendingRefresh = false;

            if (state.frame) {
                if (usedRenderer() === 'mithril') {
                    m.render(rootNode, frameFromModel(state.frame));
                } else if (usedRenderer() === 'deuce-vdom') {
                    DeuceVDom.redraw(() => frameFromModel(state.frame));
                } else {
                    let newTree = frameFromModel(state.frame),
                        patches = virtualDom.diff(vdomTree, newTree);
                    rootNode = virtualDom.patch(rootNode, patches);
                    vdomTree = newTree;
                }
            }

            console.timeEnd('redraw');
            console.timeEnd('client time');
            console.log('latency:', Date.now() - serverTime, 'ms', usedRenderer());
        });
    }
}

let handlers = {r: onrefresh, s: onpatch};

function onmessage(data) {
    debug('client received:', data.data.length, data.data);
    console.time('client time');
    console.time('onmessage');
    let message = JSON.parse(data.data),
        serverTime = message[message.length - 1];
    console.log('server time:', Date.now() - serverTime, 'ms');
    handlers[message[0]].apply(null, message.slice(1));
    console.timeEnd('onmessage');
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
    debug('connecting to', url);

    ws = new WebSocket(url);
    ws.onmessage = onmessage;
    ws.onopen = (e) => {
        debug('connection opened:', e);
        reconnectInterval = initialReconnectInterval;
    };
    ws.onerror = (e) => {
        console.error('connection error:', e);
        ws.close();
    };
    ws.onclose = (e) => {
        debug('connection closed:', e);
        debug('retrying in:', reconnectInterval, 'ms.');
        ws = undefined;
        window.setTimeout(connect, reconnectInterval);
        reconnectInterval *= reconnectBackoffRatio;
        reconnectInterval = Math.min(maxReconnectInterval, reconnectInterval);
    };
}

window.addEventListener('error', (e) => {
    if (ws) {
        console.error('error, reloading app:', e);
        ws.close();
        ws = {};
        setTimeout(() => window.location.reload(), maxReconnectInterval);
    }
});

document.addEventListener('DOMContentLoaded', () => {
    connect();
});

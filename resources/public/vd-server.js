'use strict';

// The example from https://github.com/Matt-Esch/virtual-dom split into a server rendering / client patching spike.
// virtual-dom isn't really used well, as all patching is done in text and the tree is then recrated client side.

const diff = require('diff'),
      ws = require('ws'),
      fs = require('fs');

let serialize = JSON.stringify,
    deserialize = JSON.parse;

function render(count) {
    return '<div style=\"text-align:center;line-height:' + (100 + count) +
        'px;border:1px solid red;width:' + (100 + count) + 'px;height:' +
        (100 + count) + 'px;\">' + count + '</div>';
}

let state = 0,
    html = render(state),
    revision = 0,
    connections = [];

ws.createServer({port: 8080}, (ws) => {
    connections.push(ws);
    let id = connections.length - 1,
        onrefresh = () => {
            fs.open('vd-client-bundle.js', 'r', (err, fd) => {
                if (err) {
                    throw (err);
                }
                let data = serialize({type: 'refresh', html: html, revision: revision,
                                      clientCompileTime: fs.fstatSync(fd).mtime});
                console.log(' refresh:', data);
                ws.send(data);
            });
        };
    ws.on('close', () => {
        console.log('disconnect:', id);
        connections.splice(id, 1);
    });
    ws.on('message', (data) => {
        let message = deserialize(data);
        ({refresh: onrefresh})[message.type](message);
    });
    console.log('new client:', id);
    onrefresh();
});

function toSimpleDiff(d) {
    if (d.added) {
        return d.value;
    }
    if (d.removed) {
        return -d.value.length;
    }
    return d.value.length;
}

setInterval(() => {
    state += 1;
    let newHtml = render(state),
        newRevision = revision + 1;
    console.log('rendered:', newHtml);

    console.time('    diff');
    let patch = diff.diffChars(html, newHtml).map(toSimpleDiff);
    console.timeEnd('    diff');

    if (connections.length > 0) {
        let data = serialize({type: 'patch', diff: patch, from: revision, to: newRevision});
        console.log(' sending:', data);
        connections.forEach((ws) => ws.send(data));
    }

    revision = newRevision;
    html = newHtml;
}, 1000);
